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

  df <- tidytable::as_tidytable(as.list(df))
  n <- nrow(df)

  if (!("cluster_consensus_promoted_from_anchor" %in% names(df))) {
    data.table::set(
      df,
      j = "cluster_consensus_promoted_from_anchor",
      value = rep.int(FALSE, n)
    )
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
    data.table::set(df, j = "rank_final", value = rep.int(NA_integer_, n))
    return(df)
  }

  .clean_rank_score <- function(raw_s, n_rows) {
    s <- if (is.numeric(raw_s)) {
      as.double(raw_s)
    } else {
      suppressWarnings(as.numeric(raw_s))
    }
    s <- pmax(s, 0)
    s[!is.finite(s)] <- NA_real_
    s
  }

  score_final <- .clean_rank_score(df[[score_col]], n)
  score_initial <- if ("candidate_score_pseudo_initial" %in% names(df)) {
    .clean_rank_score(df[["candidate_score_pseudo_initial"]], n)
  } else {
    rep.int(NA_real_, n)
  }

  # De-duplicate first (keeping each structure's best-scoring row in original
  # row order) so that duplicate rows do not inflate the ranks of the
  # candidates below them.
  fids <- df$feature_id
  iks <- df$candidate_structure_inchikey_connectivity_layer
  u_fids <- unique(fids)
  u_iks <- unique(iks)
  fid_id <- fastmatch::fmatch(fids, u_fids)
  ik_id <- fastmatch::fmatch(iks, u_iks)

  n_iks <- length(u_iks)
  pair_key_raw <- as.double(fid_id - 1L) * n_iks + ik_id

  if (anyDuplicated(pair_key_raw) == 0L) {
    fid_keep <- fid_id
    sf_keep <- score_final
    si_keep <- score_initial
    n_keep <- n
  } else {
    ord_dedup <- order(-score_final, na.last = TRUE, method = "radix")
    not_dup_pk <- !duplicated(pair_key_raw[ord_dedup])
    keep_rows <- sort(ord_dedup[not_dup_pk])
    df <- tidytable::as_tidytable(lapply(as.list(df), `[`, keep_rows))
    fid_keep <- fid_id[keep_rows]
    sf_keep <- score_final[keep_rows]
    si_keep <- score_initial[keep_rows]
    n_keep <- length(keep_rows)
  }

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
    seq_m <- seq_len(m)
    new_g <- c(TRUE, g_o[-1L] != g_o[-m])
    g_idx <- integer(m)
    g_idx[new_g] <- seq_m[new_g]
    g_start <- cummax(g_idx)

    new_v <- new_g | c(TRUE, v_o[-1L] != v_o[-m])
    v_tie_idx <- integer(m)
    v_tie_idx[new_v] <- seq_m[new_v]
    tie_start <- cummax(v_tie_idx)

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
