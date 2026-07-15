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

  dt <- hyps |>
    tidytable::select(
      feature_id,
      mz,
      adduct,
      evidence_cluster,
      evidence_count,
      implied_M
    ) |>
    tidytable::filter(!is.na(evidence_cluster))
  if (nrow(dt) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      adduct = character(),
      feature_id_dest = character(),
      adduct_dest = character()
    ))
  }

  dt <- dt |>
    tidytable::arrange(
      evidence_cluster,
      feature_id,
      tidytable::desc(evidence_count),
      adduct
    )

  reps <- dt |>
    tidytable::slice_head(n = 1, .by = c(evidence_cluster, feature_id)) |>
    tidytable::arrange(evidence_cluster, mz, feature_id)

  # Combine multiple assignments into single operation using lead per cluster
  reps[,
    c("feature_id_dest", "adduct_dest", "implied_M_dest") := list(
      c(as.character(feature_id[-1L]), NA_character_),
      c(as.character(adduct[-1L]), NA_character_),
      c(as.numeric(implied_M[-1L]), NA_real_)
    ),
    by = evidence_cluster
  ]

  # Only link sequential features if their implied M values are compatible
  reps <- reps |>
    tidytable::mutate(
      m_compatible = {
        m_diff <- abs(implied_M - implied_M_dest)
        m_max <- pmax(implied_M, implied_M_dest, na.rm = TRUE)
        ppm_tol <- tolerance_ppm * 1e-6 * m_max
        (m_diff <= ppm_tol) | is.na(implied_M) | is.na(implied_M_dest)
      }
    )

  out <- reps |>
    tidytable::filter(!is.na(feature_id_dest) & m_compatible) |>
    tidytable::select(feature_id, adduct, feature_id_dest, adduct_dest) |>
    tidytable::distinct()
  tidytable::as_tidytable(out)
}
