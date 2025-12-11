#' @title Filter high confidence only
#' `r lifecycle::badge("experimental")`
#'
#' @description This function filters annotation results to retain only
#'     high-confidence candidates based on multiple scoring criteria
#'     (biological, initial, chemical) and retention time accuracy.
#'
#' @include validations_utils.R
#'
#' @param df Data frame containing annotation results with score columns
#' @param score_bio_min Numeric minimum biological score threshold (default: 0.85). Range: 0-1
#' @param score_ini_min Numeric minimum initial score threshold (default: 0.95). Range: 0-1
#' @param score_final_min Numeric minimum final (chemical) score threshold (default: 0.75). Range: 0-1
#' @param error_rt_max Numeric maximum retention time error in minutes (default: 0.05). Must be > 0
#' @param confidence_sirius_min Numeric minimum SIRIUS confidence score threshold (optional).
#'     Range: 0-1. If provided, candidates WITH a confidence score below this threshold
#'     are filtered out, but candidates with NA/missing scores are retained.
#' @param similarity_spectral_min Numeric minimum spectral similarity threshold (optional).
#'     Range: 0-1. If provided, candidates WITH a similarity score below this threshold
#'     are filtered out, but candidates with NA/missing scores are retained.
#' @param matched_peaks_min Numeric minimum count of matched peaks threshold (optional).
#'     Must be >= 0 (typically a small integer). If provided, candidates WITH a matched
#'     peak count below this threshold are filtered out, but candidates with NA/missing
#'     peak counts are retained.
#' @param context Optional character string to tag logs with a stage label (e.g.,
#'     "mini+filtered" or "full"). Defaults to NULL (no tag).
#'
#' @return Data frame containing only high-confidence annotations that meet
#'     at least one score threshold and pass RT error filtering
#'
#' @details
#' Columns used when present:
#' - score_biological
#' - candidate_score_pseudo_initial
#' - score_weighted_chemo
#' - candidate_structure_error_rt (assumed in minutes; NA means unknown and is allowed)
#' - candidate_score_sirius_confidence (optional, NA values allowed)
#' - candidate_similarity (optional, NA values allowed, 0 always filtered)
#' - candidate_matched_peaks (optional, NA values allowed)
#'
#' Missing score columns are treated as absent for that criterion and do not
#' cause errors; at least one of the three primary scores must satisfy its
#' threshold for a row to be retained.
#'
#' **NA value handling:**
#' - RT error: NA values (unknown RT) are allowed to pass
#' - SIRIUS confidence: NA values (no SIRIUS annotation) are allowed to pass
#' - Spectral similarity: NA values (no spectral match) are allowed to pass, but 0 is ALWAYS filtered
#' - Matched peaks: NA values (no peak matching data) are allowed to pass
#'
#' **SIRIUS and Spectral filtering (OR logic):**
#' When both `confidence_sirius_min` and `similarity_spectral_min` are set,
#' candidates pass if EITHER the SIRIUS confidence OR spectral similarity meets
#' its threshold. This allows candidates with good SIRIUS but poor spectral match
#' (or vice versa) to pass. Spectral similarity = 0 is always filtered out as invalid.
#'
#' This means thresholds only apply when the corresponding value is present.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Filter to high-confidence annotations only
#' high_conf <- filter_high_confidence_only(
#'   df = annotations,
#'   score_bio_min = 0.85,
#'   score_ini_min = 0.95,
#'   score_final_min = 0.75,
#'   error_rt_max = 0.05,
#'   context = "final_filtered"
#' )
#'
#' # With SIRIUS confidence threshold
#' high_conf <- filter_high_confidence_only(
#'   df = annotations,
#'   confidence_sirius_min = 0.8,
#'   similarity_spectral_min = 0.7
#' )
#' }
filter_high_confidence_only <- function(
  df,
  score_bio_min = DEFAULT_HC_SCORE_BIO_MIN,
  score_ini_min = DEFAULT_HC_SCORE_INITIAL_MIN,
  score_final_min = DEFAULT_HC_SCORE_FINAL_MIN,
  error_rt_max = DEFAULT_HC_MAX_RT_ERROR_MIN,
  confidence_sirius_min = DEFAULT_HC_SCORE_SIRIUS_MIN,
  similarity_spectral_min = DEFAULT_HC_SCORE_SPECTRAL_MIN,
  matched_peaks_min = DEFAULT_HC_SCORE_MIN_PEAKS,
  context = NULL
) {
  # Validation ----
  validate_dataframe(df, param_name = "df", allow_empty = TRUE)

  if (nrow(df) == 0L) {
    log_warn("Empty data frame provided to filter")
    return(df)
  }

  validate_numeric_range(
    score_bio_min,
    param_name = "score_bio_min",
    min_value = 0,
    max_value = 1
  )
  validate_numeric_range(
    score_ini_min,
    param_name = "score_ini_min",
    min_value = 0,
    max_value = 1
  )
  validate_numeric_range(
    score_final_min,
    param_name = "score_final_min",
    min_value = 0,
    max_value = 1
  )

  if (
    !is.numeric(error_rt_max) ||
      length(error_rt_max) != 1L ||
      is.na(error_rt_max) ||
      error_rt_max <= 0
  ) {
    stop("RT error threshold must be positive (minutes)")
  }

  if (!is.null(confidence_sirius_min)) {
    validate_numeric_range(
      confidence_sirius_min,
      param_name = "confidence_sirius_min",
      min_value = 0,
      max_value = 1
    )
  }
  if (!is.null(similarity_spectral_min)) {
    validate_numeric_range(
      similarity_spectral_min,
      param_name = "similarity_spectral_min",
      min_value = 0,
      max_value = 1
    )
  }
  if (!is.null(matched_peaks_min)) {
    if (
      !is.numeric(matched_peaks_min) ||
        length(matched_peaks_min) != 1L ||
        is.na(matched_peaks_min) ||
        matched_peaks_min < 0
    ) {
      stop("Matched peaks threshold must be a non-negative number")
    }
  }

  # Prepare safe columns ----
  # IMPORTANT: We do NOT use coalesce() to convert missing values to -Inf
  # because score=NA (MS1-only hits) is a legitimate value, not missing.
  # Instead, we create columns with actual values or NA, and handle NA in filtering.
  # Explicitly convert to numeric to prevent character comparison issues.

  rt_err_vec <- if ("candidate_structure_error_rt" %in% names(df)) {
    as.numeric(df[["candidate_structure_error_rt"]])
  } else {
    rep(NA_real_, nrow(df))
  }

  df_work <- df |>
    tidytable::mutate(
      .score_bio = as.numeric(.data[["score_biological"]]),
      .score_ini = as.numeric(.data[["candidate_score_pseudo_initial"]]),
      .score_final = as.numeric(.data[["score_weighted_chemo"]]),
      .rt_err_min = rt_err_vec
    )

  n_before <- nrow(df_work)

  # Core filtering ----

  # At least one of the three score thresholds must be satisfied
  # NA values for initial score indicate MS1-only hits (no MS2 spectrum) and don't block other scores
  # Score = 0 is invalid/missing data and is filtered out
  df_filtered <- df_work |>
    tidytable::filter(
      (.score_bio >= score_bio_min) |
        (!is.na(.score_ini) & .score_ini > 0 & .score_ini >= score_ini_min) |
        (.score_final >= score_final_min)
    )

  # RT error filter (minutes). Allow NA (unknown) values
  if ("candidate_structure_error_rt" %in% names(df_filtered)) {
    df_filtered <- df_filtered |>
      tidytable::filter(
        is.na(.rt_err_min) |
          abs(
            .rt_err_min |>
              as.numeric()
          ) <=
            error_rt_max
      )
  }

  # SIRIUS confidence and spectral similarity filtering
  # ALWAYS filter out spectral similarity = 0 (invalid MS2 data) if column exists
  if ("candidate_similarity" %in% names(df_filtered)) {
    df_filtered <- df_filtered |>
      tidytable::filter(
        is.na(.data[["candidate_similarity"]]) |
          as.numeric(.data[["candidate_similarity"]]) > 0
      )
  }

  # Apply threshold filters with OR logic if set
  # NA values are allowed (no SIRIUS/spectral data available)
  has_sirius <- !is.null(confidence_sirius_min) &&
    "candidate_score_sirius_confidence" %in% names(df_filtered)
  has_spectral <- !is.null(similarity_spectral_min) &&
    "candidate_score_pseudo_initial" %in% names(df_filtered)

  if (has_sirius && has_spectral) {
    # Both filters available: use OR logic
    df_filtered <- df_filtered |>
      tidytable::filter(
        # Pass if SIRIUS is valid AND meets threshold
        (!is.na(.data[["candidate_score_sirius_confidence"]]) &
          as.numeric(.data[["candidate_score_sirius_confidence"]]) >=
            confidence_sirius_min) |
          # OR spectral is valid (not NA, already filtered >0) AND meets threshold
          (!is.na(.data[["candidate_score_pseudo_initial"]]) &
            as.numeric(.data[["candidate_score_pseudo_initial"]]) >=
              similarity_spectral_min) |
          # OR both are NA (no MS2 data at all)
          (is.na(.data[["candidate_score_sirius_confidence"]]) &
            is.na(.data[["candidate_score_pseudo_initial"]]))
      )
  } else if (has_sirius) {
    # Only SIRIUS filter
    df_filtered <- df_filtered |>
      tidytable::filter(
        is.na(.data[["candidate_score_sirius_confidence"]]) |
          as.numeric(.data[["candidate_score_sirius_confidence"]]) >=
            confidence_sirius_min
      )
  } else if (has_spectral) {
    # Only spectral filter (already filtered >0 above)
    df_filtered <- df_filtered |>
      tidytable::filter(
        is.na(.data[["candidate_score_pseudo_initial"]]) |
          as.numeric(.data[["candidate_score_pseudo_initial"]]) >=
            similarity_spectral_min
      )
  }

  # Matched peaks filtering
  # ALWAYS filter out 0 matched peaks (invalid data - no peaks matched)
  # NA values (missing peak count data) are allowed to pass
  if ("candidate_count_similarity_peaks_matched" %in% names(df_filtered)) {
    df_filtered <- df_filtered |>
      tidytable::filter(
        is.na(.data[["candidate_count_similarity_peaks_matched"]]) |
          as.numeric(.data[["candidate_count_similarity_peaks_matched"]]) > 0
      )
  }

  # Apply matched peaks threshold if provided
  if (
    !is.null(matched_peaks_min) &&
      "candidate_count_similarity_peaks_matched" %in% names(df_filtered)
  ) {
    df_filtered <- df_filtered |>
      tidytable::filter(
        is.na(.data[["candidate_count_similarity_peaks_matched"]]) |
          as.numeric(.data[["candidate_count_similarity_peaks_matched"]]) >=
            matched_peaks_min
      )
  }

  # Drop helper columns
  df_filtered <- df_filtered |>
    tidytable::select(
      -tidyselect::starts_with(match = ".score_"),
      -tidyselect::starts_with(match = ".rt_err_")
    )

  n_after <- nrow(df_filtered)
  n_removed <- n_before - n_after
  percent_removed <- round(100 * n_removed / n_before, 1)

  # Build a tag for log lines if context is provided
  tag <- if (!is.null(context) && nzchar(context)) {
    paste0("[", context, "] ")
  } else {
    ""
  }

  log_info(
    "%s Removed %d low-confidence candidates (%s%% of %d total)",
    tag,
    n_removed,
    percent_removed,
    n_before
  )
  log_info(
    "%s %d high-confidence candidates remaining (%s%%)",
    tag,
    n_after,
    round(100 * n_after / n_before, 1)
  )

  df_filtered
}
