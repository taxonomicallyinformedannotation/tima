#' Rank candidates by final score
#'
#' @description
#' Ranks candidates by descending final score per feature.
#'
#' For each feature_id, candidates are ranked 1 (highest score_weighted_chemo)
#' downward. When scores are identical, promoted candidates are listed first.
#'
#' @param df Data frame with columns:
#'   - feature_id
#'   - score_weighted_chemo or score_final (0-1, combined evidence)
#'   - cluster_consensus_promoted_from_anchor (logical, optional)
#'
#' @return Data frame with added `rank_final` column
#'
#' @keywords internal
rank_candidates <- function(df) {
  if (nrow(df) == 0L) {
    return(df)
  }

  # Ensure promotion flag exists (optional)
  if (!("cluster_consensus_promoted_from_anchor" %in% names(df))) {
    df$cluster_consensus_promoted_from_anchor <- FALSE
  }

  # If no score column found, return with NA ranks
  score_col <- if ("score_weighted_chemo" %in% names(df)) {
    "score_weighted_chemo"
  } else if ("score_final" %in% names(df)) {
    "score_final"
  } else {
    warning(
      "Neither score_weighted_chemo nor score_final found; ranking may be incorrect."
    )
    NULL
  }
  if (is.null(score_col)) {
    df$rank_final <- NA_integer_
    return(df)
  }

  df$score_final <- suppressWarnings(as.numeric(df[[score_col]]))
  df$score_final <- pmax(df$score_final, 0)
  df$score_final[!is.finite(df$score_final)] <- NA_real_

  df$score_initial <- if ("candidate_score_pseudo_initial" %in% names(df)) {
    suppressWarnings(as.numeric(df[["candidate_score_pseudo_initial"]]))
  } else {
    rep(NA_real_, nrow(df))
  }
  df$score_initial <- pmax(df$score_initial, 0)
  df$score_initial[!is.finite(df$score_initial)] <- NA_real_

  # Rank within each feature_id by score_final (descending), breaking ties by promoted status
  df <- tidytable::as_tidytable(df) |>
    tidytable::mutate(
      rank_final = tidytable::min_rank(-score_final),
      rank_initial = tidytable::min_rank(-score_initial),
      .by = feature_id
    ) |>
    tidytable::distinct(
      feature_id,
      candidate_structure_inchikey_connectivity_layer,
      .keep_all = TRUE
    ) |>
    tidytable::select(-score_final, -score_initial)

  df
}
