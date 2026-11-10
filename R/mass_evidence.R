empty_evidence_table <- function() {
  tidytable::tidytable(
    feature_id = character(),
    rt = numeric(),
    mz = numeric(),
    sample = character(),
    adduct = character(),
    n_mer = integer(),
    z = integer(),
    adduct_mass = numeric(),
    n_iso = integer(),
    implied_M = numeric(),
    nearest_mass_error_ppm = numeric(),
    mass_cluster = integer(),
    rt_cluster = integer(),
    evidence_cluster = character(),
    n_evidence_features = integer(),
    evidence_count = integer(),
    evidence_score = integer(),
    candidate_adduct_origin = character(),
    source = character()
  )
}

#' Build adduct co-occurrence edges from an evidence-supported hypothesis
#' table.
#'
#' To remain scalable, each (evidence_cluster, feature) is reduced to one
#' representative adduct (the highest-scoring), then features are linked as
#' an m/z chain inside the cluster — O(n) edges per cluster instead of O(n^2).
#'
#' @keywords internal
build_evidence_edges <- function(
  hyps,
  tolerance_ppm = 5,
  tolerance_dalton = NULL
) {
  if (nrow(hyps) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      adduct = character(),
      feature_id_dest = character(),
      adduct_dest = character()
    ))
  }

  hyps <- tidytable::as_tidytable(hyps)
  if (!"implied_M" %in% colnames(hyps)) {
    hyps$implied_M <- NA_real_
  }

  ev_cl <- hyps$evidence_cluster
  keep_cl <- !is.na(ev_cl)
  if ("evidence_count" %in% colnames(hyps)) {
    keep_cl <- keep_cl &
      (is.na(hyps$evidence_count) | hyps$evidence_count >= 1L)
  }
  if (!any(keep_cl)) {
    return(tidytable::tidytable(
      feature_id = character(),
      adduct = character(),
      feature_id_dest = character(),
      adduct_dest = character()
    ))
  }

  idx_cl <- which(keep_cl)
  cl_sub <- as.character(ev_cl[idx_cl])
  dup_cl <- unique(cl_sub[duplicated(cl_sub)])
  if (length(dup_cl) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      adduct = character(),
      feature_id_dest = character(),
      adduct_dest = character()
    ))
  }
  idx_cl <- idx_cl[!is.na(fastmatch::fmatch(cl_sub, dup_cl))]

  f_sub <- as.character(hyps$feature_id[idx_cl])
  mz_sub <- as.numeric(hyps$mz[idx_cl])
  add_sub <- as.character(hyps$adduct[idx_cl])
  cl_sub <- as.character(ev_cl[idx_cl])
  cnt_sub <- as.integer(hyps$evidence_count[idx_cl])
  cnt_sub[is.na(cnt_sub)] <- 0L
  m_sub <- as.numeric(hyps$implied_M[idx_cl])

  u_cl <- sort(unique(cl_sub), method = "radix")
  u_f <- sort(unique(f_sub), method = "radix")
  u_add <- sort(unique(add_sub), method = "radix")
  cl_id <- fastmatch::fmatch(cl_sub, u_cl)
  f_id <- fastmatch::fmatch(f_sub, u_f)
  add_id <- fastmatch::fmatch(add_sub, u_add)

  ord1 <- order(cl_id, f_id, -cnt_sub, add_id, method = "radix")
  cl_id <- cl_id[ord1]
  f_id <- f_id[ord1]
  mz_sub <- mz_sub[ord1]
  add_id <- add_id[ord1]
  m_sub <- m_sub[ord1]

  n1 <- length(cl_id)
  keep_rep <- c(
    TRUE,
    cl_id[-1L] != cl_id[-n1] | f_id[-1L] != f_id[-n1]
  )
  cl_rep <- cl_id[keep_rep]
  f_rep <- f_id[keep_rep]
  mz_rep <- mz_sub[keep_rep]
  add_rep <- add_id[keep_rep]
  m_rep <- m_sub[keep_rep]

  n_rep <- length(cl_rep)
  if (n_rep < 2L) {
    return(tidytable::tidytable(
      feature_id = character(),
      adduct = character(),
      feature_id_dest = character(),
      adduct_dest = character()
    ))
  }

  ord2 <- order(cl_rep, mz_rep, f_rep, method = "radix")
  cl_rep <- cl_rep[ord2]
  f_rep <- f_rep[ord2]
  add_rep <- add_rep[ord2]
  m_rep <- m_rep[ord2]

  same_cl <- which(cl_rep[-n_rep] == cl_rep[-1L])
  if (length(same_cl) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      adduct = character(),
      feature_id_dest = character(),
      adduct_dest = character()
    ))
  }

  m_src <- m_rep[same_cl]
  m_dst <- m_rep[same_cl + 1L]
  m_diff <- abs(m_src - m_dst)
  m_max <- pmax.int(m_src, m_dst)
  na_m <- is.na(m_src) | is.na(m_dst)
  ppm_tol <- tolerance_ppm * 1e-6 * m_max
  tol <- if (!is.null(tolerance_dalton) && is.finite(tolerance_dalton)) {
    pmax(ppm_tol, as.numeric(tolerance_dalton))
  } else {
    ppm_tol
  }
  m_ok <- na_m | (m_diff <= tol)
  keep_idx <- same_cl[m_ok]
  if (length(keep_idx) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      adduct = character(),
      feature_id_dest = character(),
      adduct_dest = character()
    ))
  }

  f1_out <- f_rep[keep_idx]
  a1_out <- add_rep[keep_idx]
  f2_out <- f_rep[keep_idx + 1L]
  a2_out <- add_rep[keep_idx + 1L]
  attr(u_f, ".match.hash") <- NULL
  attr(u_add, ".match.hash") <- NULL

  tidytable::tidytable(
    feature_id = u_f[f1_out],
    adduct = u_add[a1_out],
    feature_id_dest = u_f[f2_out],
    adduct_dest = u_add[a2_out]
  ) |>
    tidytable::distinct()
}
