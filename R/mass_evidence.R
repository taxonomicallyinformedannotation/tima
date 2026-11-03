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
build_evidence_edges <- function(hyps, tolerance_ppm = 5) {
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
  if (length(idx_cl) < 2L) {
    return(tidytable::tidytable(
      feature_id = character(),
      adduct = character(),
      feature_id_dest = character(),
      adduct_dest = character()
    ))
  }

  f_sub <- as.character(hyps$feature_id[idx_cl])
  mz_sub <- as.numeric(hyps$mz[idx_cl])
  add_sub <- as.character(hyps$adduct[idx_cl])
  cl_sub <- as.character(ev_cl[idx_cl])
  cnt_sub <- as.integer(hyps$evidence_count[idx_cl])
  cnt_sub[is.na(cnt_sub)] <- 0L
  m_sub <- as.numeric(hyps$implied_M[idx_cl])

  ord1 <- order(cl_sub, f_sub, -cnt_sub, add_sub, method = "radix")
  cl_sub <- cl_sub[ord1]
  f_sub <- f_sub[ord1]
  mz_sub <- mz_sub[ord1]
  add_sub <- add_sub[ord1]
  m_sub <- m_sub[ord1]

  n1 <- length(cl_sub)
  keep_rep <- c(
    TRUE,
    cl_sub[-1L] != cl_sub[-n1] | f_sub[-1L] != f_sub[-n1]
  )
  cl_rep <- cl_sub[keep_rep]
  f_rep <- f_sub[keep_rep]
  mz_rep <- mz_sub[keep_rep]
  add_rep <- add_sub[keep_rep]
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
  m_max <- pmax(m_src, m_dst, na.rm = TRUE)
  ppm_tol <- tolerance_ppm * 1e-6 * m_max
  m_ok <- is.na(m_src) | is.na(m_dst) | (m_diff <= ppm_tol)
  keep_idx <- same_cl[m_ok]
  if (length(keep_idx) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      adduct = character(),
      feature_id_dest = character(),
      adduct_dest = character()
    ))
  }

  tidytable::tidytable(
    feature_id = f_rep[keep_idx],
    adduct = add_rep[keep_idx],
    feature_id_dest = f_rep[keep_idx + 1L],
    adduct_dest = add_rep[keep_idx + 1L]
  ) |>
    tidytable::distinct()
}
