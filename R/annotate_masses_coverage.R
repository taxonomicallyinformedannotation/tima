# ============================================================
#                       STEP HELPERS
# ============================================================

#' Resolve annotate_masses coverage mode
#' @keywords internal
resolve_annotate_masses_coverage_mode <- function(coverage_mode) {
  mode <- coverage_mode
  if (is.null(mode) || (length(mode) == 1L && is.na(mode))) {
    mode <- "best_supported_conflict_free"
  }
  mode <- tolower(as.character(mode[[1L]]))
  allowed <- c("best_supported_conflict_free", "broad_conflict_free")
  if (!mode %in% allowed) {
    log_warn(
      "Invalid coverage_mode='%s', falling back to 'best_supported_conflict_free'",
      mode
    )
    mode <- "best_supported_conflict_free"
  }
  mode
}


#' Write the empty-output sentinel files
#' @keywords internal
write_empty_annotate_masses_outputs <- function(
  output_annotations,
  output_edges
) {
  ann_cols <- c(
    "feature_id",
    "candidate_structure_error_mz",
    "candidate_structure_name",
    "candidate_structure_inchikey_connectivity_layer",
    "candidate_structure_smiles_no_stereo",
    "candidate_structure_molecular_formula",
    "candidate_structure_exact_mass",
    "candidate_structure_xlogp",
    "candidate_structure_tag",
    "candidate_library",
    "candidate_structure_tax_npc_01pat",
    "candidate_structure_tax_npc_02sup",
    "candidate_structure_tax_npc_03cla",
    "candidate_structure_tax_cla_chemontid",
    "candidate_structure_tax_cla_01kin",
    "candidate_structure_tax_cla_02sup",
    "candidate_structure_tax_cla_03cla",
    "candidate_structure_tax_cla_04dirpar",
    "candidate_adduct",
    "candidate_adduct_origin",
    "candidate_annotation_level",
    "candidate_evidence_tier",
    "adduct_support",
    "annotation_note"
  )
  empty_annotations <- as.data.frame(
    matrix(ncol = length(ann_cols), nrow = 0L)
  )
  colnames(empty_annotations) <- ann_cols

  edge_cols <- c("CLUSTERID1", "CLUSTERID2", "label")
  empty_edges <- as.data.frame(matrix(ncol = length(edge_cols), nrow = 0L))
  colnames(empty_edges) <- edge_cols

  empty_coverage <- build_annotate_masses_coverage_report(
    annotations = tidytable::tidytable(),
    baseline_adduct = NA_character_
  )

  export_output(x = empty_annotations, file = output_annotations)
  export_output(x = empty_edges, file = output_edges)
  export_output(
    x = empty_coverage,
    file = derive_annotate_masses_coverage_path(output_annotations[[1L]])
  )
  c(
    "annotations" = output_annotations[[1L]],
    "edges" = output_edges[[1L]]
  )
}

#' Derive the annotate_masses coverage-report file path
#' @keywords internal
derive_annotate_masses_coverage_path <- function(output_annotations) {
  output_annotations <- as.character(output_annotations)[1L]
  if (is.na(output_annotations) || !nzchar(output_annotations)) {
    return("annotate_masses_coverage.tsv")
  }
  if (grepl("\\.tsv(\\.gz)?$", output_annotations)) {
    return(sub("\\.tsv(\\.gz)?$", "_coverage.tsv\\1", output_annotations))
  }
  paste0(output_annotations, "_coverage.tsv")
}

#' Build a feature-level coverage report for annotate_masses
#' @keywords internal
#' @include logs_utils.R
#' @include export_output.R
build_annotate_masses_coverage_report <- function(
  annotations,
  baseline_adduct
) {
  if (nrow(annotations) == 0L) {
    return(tidytable::tidytable(
      coverage_scope = c("best", "any"),
      coverage_class = c("all", "all"),
      coverage_tier = c(0L, 0L),
      N_features = c(0L, 0L),
      N_annotations = c(0L, 0L),
      Pct_features = c("0.00%", "0.00%"),
      Pct_annotations = c("0.00%", "0.00%")
    ))
  }

  n_rows <- nrow(annotations)
  f_col <- annotations[["feature_id"]]
  src_col <- if ("source" %in% names(annotations)) {
    as.character(annotations[["source"]])
  } else {
    rep(NA_character_, n_rows)
  }
  add_col <- if ("adduct" %in% names(annotations)) {
    as.character(annotations[["adduct"]])
  } else {
    rep(NA_character_, n_rows)
  }
  has_str <- if ("candidate_structure_error_mz" %in% names(annotations)) {
    !is.na(annotations[["candidate_structure_error_mz"]])
  } else {
    logical(n_rows)
  }

  baseline_active <- !is.null(baseline_adduct) &&
    length(baseline_adduct) >= 1L &&
    !is.na(baseline_adduct[[1L]]) &&
    nzchar(baseline_adduct[[1L]])
  baseline_label <- if (baseline_active) {
    baseline_adduct[[1L]]
  } else {
    NA_character_
  }

  tier <- rep(7L, n_rows)
  if (baseline_active) {
    tier[!is.na(add_col) & add_col == baseline_label] <- 6L
  }
  tier[!is.na(src_col) & src_col == "evidence"] <- 5L
  tier[!is.na(src_col) & src_col %in% c("cluster", "loss")] <- 4L
  tier[!is.na(src_col) & src_col == "multi"] <- 3L
  tier[
    !is.na(src_col) &
      src_col %in% c("pair", "preassigned", "preassigned_propagated")
  ] <- 2L
  tier[has_str] <- 1L

  u_f <- unique(f_col)
  all_features <- length(u_f)
  all_annotations <- n_rows
  f_id <- fastmatch::fmatch(f_col, u_f)
  feat_n_ann <- tabulate(f_id, nbins = all_features)

  tier_classes <- c(
    "structure_matched",
    "pairwise_supported",
    "evidence_multicharge_supported",
    "modifier_pairwise_supported",
    "evidence_supported",
    "baseline_fallback",
    "adduct_only"
  )

  best_tier_per_feat <- rep(8L, all_features)
  any_n_feat <- integer(7L)
  any_n_ann <- tabulate(tier, nbins = 7L)
  for (t in 1L:7L) {
    if (any_n_ann[t] > 0L) {
      f_in_t <- tabulate(f_id[tier == t], nbins = all_features) > 0L
      any_n_feat[t] <- sum(f_in_t)
      best_tier_per_feat[f_in_t & best_tier_per_feat > t] <- t
    }
  }

  best_n_feat <- tabulate(best_tier_per_feat, nbins = 7L)
  best_n_ann <- integer(7L)
  for (t in which(best_n_feat > 0L)) {
    best_n_ann[t] <- sum(feat_n_ann[best_tier_per_feat == t])
  }

  keep_best <- which(best_n_feat > 0L)
  best_summary <- tidytable::tidytable(
    coverage_class = tier_classes[keep_best],
    coverage_tier = keep_best,
    N_features = best_n_feat[keep_best],
    N_annotations = best_n_ann[keep_best],
    coverage_scope = "best"
  )

  keep_any <- which(any_n_ann > 0L)
  any_summary <- tidytable::tidytable(
    coverage_class = tier_classes[keep_any],
    coverage_tier = keep_any,
    N_features = any_n_feat[keep_any],
    N_annotations = any_n_ann[keep_any],
    coverage_scope = "any"
  )

  all_rows <- tidytable::tidytable(
    coverage_scope = c("best", "any"),
    coverage_class = c("all", "all"),
    coverage_tier = c(0L, 0L),
    N_features = c(all_features, all_features),
    N_annotations = c(all_annotations, all_annotations)
  )

  coverage_report <- tidytable::bind_rows(
    all_rows,
    best_summary,
    any_summary
  )

  coverage_report$Pct_features <- sprintf(
    "%.2f%%",
    100 * coverage_report$N_features / max(1L, all_features)
  )
  coverage_report$Pct_annotations <- sprintf(
    "%.2f%%",
    100 * coverage_report$N_annotations / all_annotations
  )

  coverage_report <- coverage_report |>
    tidytable::select(
      coverage_scope,
      coverage_class,
      coverage_tier,
      N_features,
      N_annotations,
      Pct_features,
      Pct_annotations
    ) |>
    tidytable::arrange(coverage_scope, coverage_tier, coverage_class)

  coverage_report
}
