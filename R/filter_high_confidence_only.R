#' @title Filter high confidence only
#' `r lifecycle::badge("experimental")`
#'
#' @description This function filters annotation results to retain only
#'     high-confidence candidates based on multiple scoring criteria
#'     (biological, initial, chemical) and retention time accuracy.
#'
#' @param df Data frame containing annotation results with score columns
#' @param score_bio_min Numeric minimum biological score threshold (default: 0.85)
#' @param score_ini_min Numeric minimum initial score threshold (default: 0.95)
#' @param score_final_min Numeric minimum final (chemical) score threshold (default: 0.75)
#' @param error_rt_max Numeric maximum retention time error in minutes (default: 0.1)
#' @param confidence_sirius_min Numeric minimum SIRIUS confidence score threshold (optional)
#' @param similarity_spectral_min Numeric minimum spectral similarity threshold (optional)
#'
#' @return Data frame containing only high-confidence annotations that meet
#'     at least one score threshold and pass RT error filtering
#'
#' @examples NULL
filter_high_confidence_only <- function(
  df,
  score_bio_min = DEFAULT_HC_SCORE_BIO_MIN,
  score_ini_min = DEFAULT_HC_SCORE_INITIAL_MIN,
  score_final_min = DEFAULT_HC_SCORE_FINAL_MIN,
  error_rt_max = DEFAULT_HC_MAX_RT_ERROR_MIN,
  confidence_sirius_min = NULL,
  similarity_spectral_min = NULL
) {
  # Validate inputs
  if (!is.data.frame(df) && !inherits(df, "tbl")) {
    stop("Input 'df' must be a data frame or tibble")
  }

  if (nrow(df) == 0L) {
    logger::log_warn("Empty data frame provided to filter")
    return(df)
  }

  # Validate score thresholds are in valid range
  if (
    any(
      c(score_bio_min, score_ini_min, score_final_min) < 0 |
        c(score_bio_min, score_ini_min, score_final_min) > 1
    )
  ) {
    stop("Score thresholds must be between 0 and 1")
  }

  if (error_rt_max <= 0) {
    stop("RT error threshold must be positive")
  }

  if (
    !is.null(confidence_sirius_min) &&
      (confidence_sirius_min < 0 || confidence_min > 1)
  ) {
    stop("confidence_sirius_min must be between 0 and 1")
  }
  if (
    !is.null(similarity_spectral_min) &&
      (similarity_spectral_min < 0 || similarity_min > 1)
  ) {
    stop("similarity_spectral_min must be between 0 and 1")
  }

  # logger::log_trace("Filtering for high-confidence candidates")

  n_before <- nrow(df)

  # Filter by score thresholds (at least one must be met)
  # TODO: This is basic filtering. Future improvements could add:
  # - SIRIUS confidence scores (NULL by default)
  # - Internal library match quality
  # - Spectral similarity thresholds (NULL by default)
  df_filtered <- df |>
    tidytable::filter(
      score_biological >= score_bio_min |
        candidate_score_pseudo_initial >= score_ini_min |
        score_weighted_chemo >= score_final_min
    )

  # Optional: SIRIUS confidence filter if column present and threshold provided
  if (!is.null(confidence_min)) {
    conf_col <- tidytable::coalesce(
      tidyselect::vars_select(
        names(df_filtered),
        tidyselect::any_of("candidate_score_sirius_confidence")
      ),
      character(0)
    )
    if (length(conf_col) == 1 && conf_col %in% names(df_filtered)) {
      df_filtered <- df_filtered |>
        tidytable::filter(.data[[conf_col]] >= confidence_sirius_min)
    }
  }

  # Optional: spectral similarity filter if column present and threshold provided
  if (
    !is.null(similarity_spectral_min) &&
      "candidate_similarity" %in% names(df_filtered)
  ) {
    df_filtered <- df_filtered |>
      tidytable::filter(candidate_similarity >= similarity_spectral_min)
  }

  # Apply RT error filter if column exists
  if ("candidate_structure_error_rt" %in% colnames(df_filtered)) {
    # logger::log_trace("Applying RT error filter (max: {error_rt_max} min)")
    df_filtered <- df_filtered |>
      tidytable::filter(
        is.na(candidate_structure_error_rt) |
          candidate_structure_error_rt < error_rt_max
      )
  }

  n_after <- nrow(df_filtered)
  n_removed <- n_before - n_after
  percent_removed <- round(100 * n_removed / n_before, 1)

  logger::log_info(
    "Removed ",
    n_removed,
    " low-confidence candidates (",
    percent_removed,
    "% of ",
    n_before,
    " total)"
  )
  logger::log_info(
    n_after,
    " high-confidence candidates remaining (",
    round(100 * n_after / n_before, 1),
    "%)"
  )

  return(df_filtered)
}
