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

  n <- nrow(df)
  score_final <- suppressWarnings(as.numeric(df[[score_col]]))
  score_final <- pmax(score_final, 0)
  score_final[!is.finite(score_final)] <- NA_real_

  score_initial <- if ("candidate_score_pseudo_initial" %in% names(df)) {
    suppressWarnings(as.numeric(df[["candidate_score_pseudo_initial"]]))
  } else {
    rep.int(NA_real_, n)
  }
  score_initial <- pmax(score_initial, 0)
  score_initial[!is.finite(score_initial)] <- NA_real_

  # De-duplicate first (keeping each structure's best-scoring row in original
  # row order) so that duplicate rows do not inflate the ranks of the
  # candidates below them.
  fids <- df$feature_id
  iks <- df$candidate_structure_inchikey_connectivity_layer
  u_fids <- unique(fids)
  u_iks <- unique(iks)
  fid_id <- fastmatch::fmatch(fids, u_fids)
  ik_id <- fastmatch::fmatch(iks, u_iks)

  row_id <- seq_len(n)
  ord_dedup <- order(-score_final, row_id, na.last = TRUE, method = "radix")
  n_iks <- max(1L, length(u_iks))
  if (as.numeric(length(u_fids)) * as.numeric(n_iks) < 4.5e15) {
    pair_key <- as.numeric(fid_id[ord_dedup] - 1L) * n_iks + ik_id[ord_dedup]
    keep_rows <- sort(ord_dedup[!duplicated(pair_key)])
  } else {
    pair_df <- data.frame(f = fid_id[ord_dedup], k = ik_id[ord_dedup])
    keep_rows <- sort(ord_dedup[!duplicated(pair_df)])
  }

  df <- tidytable::as_tidytable(df[keep_rows, , drop = FALSE])
  fid_keep <- fid_id[keep_rows]
  sf_keep <- score_final[keep_rows]
  si_keep <- score_initial[keep_rows]
  n_keep <- length(keep_rows)

  .fast_min_rank_desc <- function(grp_id, val, n_rows) {
    valid <- !is.na(val)
    out <- rep.int(NA_integer_, n_rows)
    if (!any(valid)) {
      return(out)
    }
    v_idx <- which(valid)
    g <- grp_id[v_idx]
    v <- -val[v_idx]
    ord <- order(g, v, method = "radix")
    g_o <- g[ord]
    v_o <- v[ord]
    m <- length(ord)
    if (m == 1L) {
      out[v_idx[ord]] <- 1L
      return(out)
    }
    new_g <- c(TRUE, g_o[-1L] != g_o[-m])
    g_start <- cummax(ifelse(new_g, seq_len(m), 1L))
    pos_in_g <- seq_len(m) - g_start + 1L
    new_v <- new_g | c(TRUE, v_o[-1L] != v_o[-m])
    rk <- cummax(ifelse(new_v, pos_in_g, 1L))
    # Because rk can decrease across groups if we use global cummax, reset per group:
    # Actually: global tie-start index is `tie_start <- cummax(ifelse(new_v, seq_len(m), 1L))`!
    tie_start <- cummax(ifelse(new_v, seq_len(m), 1L))
    out[v_idx[ord]] <- tie_start - g_start + 1L
    out
  }

  data.table::set(
    df,
    j = "rank_final",
    value = .fast_min_rank_desc(fid_keep, sf_keep, n_keep)
  )
  data.table::set(
    df,
    j = "rank_initial",
    value = .fast_min_rank_desc(fid_keep, si_keep, n_keep)
  )
  for (drop_col in intersect(c("score_final", "score_initial"), names(df))) {
    data.table::set(df, j = drop_col, value = NULL)
  }
  df
}
