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
#'
#' @return Data frame containing only high-confidence annotations that meet
#'     at least one score threshold and pass RT error filtering
#'
#' @examples NULL
filter_high_confidence_only <- function(
  df,
  score_bio_min = 0.85,
  score_ini_min = 0.95,
  score_final_min = 0.75,
  error_rt_max = 0.1
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
  if (any(c(score_bio_min, score_ini_min, score_final_min) < 0 |
          c(score_bio_min, score_ini_min, score_final_min) > 1)) {
    stop("Score thresholds must be between 0 and 1")
  }

  if (error_rt_max <= 0) {
    stop("RT error threshold must be positive")
  }

  logger::log_trace("Filtering for high-confidence candidates")

  n_before <- nrow(df)

  # Filter by score thresholds (at least one must be met)
  # TODO: This is basic filtering. Future improvements could add:
  # - SIRIUS confidence scores
  # - Internal library match quality
  # - Spectral similarity thresholds
  df_filtered <- df |>
    tidytable::filter(
      score_biological >= score_bio_min |
        candidate_score_pseudo_initial >= score_ini_min |
        score_weighted_chemo >= score_final_min
    )

  # Apply RT error filter if column exists
  if ("candidate_structure_error_rt" %in% colnames(df_filtered)) {
    logger::log_trace("Applying RT error filter (max: ", error_rt_max, " min)")
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
    "Removed ", n_removed, " low-confidence candidates (",
    percent_removed, "% of ", n_before, " total)"
  )
  logger::log_info(
    n_after, " high-confidence candidates remaining (",
    round(100 * n_after / n_before, 1), "%)"
  )

  return(df_filtered)
}
