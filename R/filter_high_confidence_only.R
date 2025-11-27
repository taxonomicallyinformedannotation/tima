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
#' @param confidence_sirius_min Numeric minimum SIRIUS confidence score threshold (optional). Range: 0-1
#' @param similarity_spectral_min Numeric minimum spectral similarity threshold (optional). Range: 0-1
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
#' - candidate_score_sirius_confidence (optional)
#' - candidate_similarity (optional)
#'
#' Missing score columns are treated as absent for that criterion and do not
#' cause errors; at least one of the three primary scores must satisfy its
#' threshold for a row to be retained.
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
  context = NULL
) {
  # Validation ----
  validate_dataframe(df, param_name = "df", allow_empty = TRUE)

  if (nrow(df) == 0L) {
    logger::log_warn("Empty data frame provided to filter")
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

  # Prepare safe columns ----
  rt_err_vec <- if ("candidate_structure_error_rt" %in% names(df)) {
    df[["candidate_structure_error_rt"]]
  } else {
    rep(NA_real_, nrow(df))
  }

  df_work <- df |>
    tidytable::mutate(
      .score_bio = tidytable::coalesce(.data[["score_biological"]], -Inf),
      .score_ini = tidytable::coalesce(
        .data[["candidate_score_pseudo_initial"]],
        -Inf
      ),
      .score_final = tidytable::coalesce(.data[["score_weighted_chemo"]], -Inf),
      .rt_err_min = rt_err_vec
    )

  n_before <- nrow(df_work)

  # Core filtering ----

  # At least one of the three score thresholds must be satisfied
  df_filtered <- df_work |>
    tidytable::filter(
      (.score_bio >= score_bio_min) |
        (.score_ini >= score_ini_min) |
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

  # Optional SIRIUS confidence
  if (
    !is.null(confidence_sirius_min) &&
      "candidate_score_sirius_confidence" %in% names(df_filtered)
  ) {
    df_filtered <- df_filtered |>
      tidytable::filter(
        .data[["candidate_score_sirius_confidence"]] >= confidence_sirius_min
      )
  }

  # Optional spectral similarity
  if (
    !is.null(similarity_spectral_min) &&
      "candidate_similarity" %in% names(df_filtered)
  ) {
    df_filtered <- df_filtered |>
      tidytable::filter(
        .data[["candidate_similarity"]] >= similarity_spectral_min
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

  logger::log_info(
    tag,
    "Removed ",
    n_removed,
    " low-confidence candidates (",
    percent_removed,
    "% of ",
    n_before,
    " total)"
  )
  logger::log_info(
    tag,
    n_after,
    " high-confidence candidates remaining (",
    round(100 * n_after / n_before, 1),
    "%)"
  )

  df_filtered
}
