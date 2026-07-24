#' Rank candidates by final score
#'
#' @description
#' Ranks candidates by descending final score per feature.
#' Ties broken by cluster consensus promotion (promoted candidates rank first).
#'
#' For each feature_id, candidates are ranked 1 (highest score_weighted_chemo) downward.
#' When scores are identical, promoted candidates (cluster_consensus_promoted_from_anchor = TRUE)
#' receive the same rank but are listed first.
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

  # Use score_weighted_chemo (primary) or fallback to score_final
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

  # Ensure promotion flag exists (optional)
  if (!("cluster_consensus_promoted_from_anchor" %in% names(df))) {
    df$cluster_consensus_promoted_from_anchor <- FALSE
  }

  # If no score column found, return with NA ranks
  if (is.null(score_col)) {
    df$rank_final <- NA_integer_
    return(df)
  }

  # Clean and coerce values
  score_final <- suppressWarnings(as.numeric(df[[score_col]]))
  score_final <- pmax(score_final, 0)
  score_final[!is.finite(score_final)] <- NA_real_

  promoted <- suppressWarnings(as.logical(df[[
    "cluster_consensus_promoted_from_anchor"
  ]]))
  promoted[is.na(promoted)] <- FALSE

  # Group by feature_id and rank by descending score
  feature_ids <- as.character(df[["feature_id"]])
  df$rank_final <- NA_integer_

  groups <- split(seq_len(nrow(df)), feature_ids)
  for (fid in names(groups)) {
    idx <- groups[[fid]]
    if (length(idx) > 0L) {
      # Order by descending score, then promoted first
      order_idx <- order(
        -score_final[idx], # Primary: higher score first
        !promoted[idx], # Tiebreaker: promoted first (FALSE before TRUE)
        na.last = TRUE,
        method = "radix"
      )
      # Rank by descending score, ties get same rank
      ranks <- rank(-score_final[idx[order_idx]], ties.method = "min")
      df$rank_final[idx[order_idx]] <- ranks
    }
  }

  df
}
