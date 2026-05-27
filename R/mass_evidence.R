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
build_evidence_edges <- function(hyps) {
  if (nrow(hyps) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      adduct = character(),
      feature_id_dest = character(),
      adduct_dest = character()
    ))
  }

  dt <- tidytable::as_tidytable(hyps)[,
    .(feature_id, mz, adduct, evidence_cluster, evidence_count)
  ]
  dt <- dt[!is.na(evidence_cluster)]
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
  reps <- dt[, .SD[1L], by = .(evidence_cluster, feature_id)]
  reps <- reps |>
    tidytable::arrange(evidence_cluster, mz, feature_id)

  reps[,
    feature_id_dest := c(as.character(feature_id[-1L]), NA_character_),
    by = evidence_cluster
  ]
  reps[,
    adduct_dest := c(as.character(adduct[-1L]), NA_character_),
    by = evidence_cluster
  ]

  out <- reps[
    !is.na(feature_id_dest),
    .(feature_id, adduct, feature_id_dest, adduct_dest)
  ]
  out <- unique(out)
  tidytable::as_tidytable(out)
}
