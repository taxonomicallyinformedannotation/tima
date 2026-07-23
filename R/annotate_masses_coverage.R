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

  support_ranked <- tidytable::as_tidytable(
    as.data.frame(annotations, stringsAsFactors = FALSE)
  )
  if (!"source" %in% names(support_ranked)) {
    support_ranked$source <- NA_character_
  }
  if (!"adduct" %in% names(support_ranked)) {
    support_ranked$adduct <- NA_character_
  }
  if (!"candidate_structure_error_mz" %in% names(support_ranked)) {
    support_ranked$candidate_structure_error_mz <- NA_real_
  }
  if (!"adduct_support" %in% names(support_ranked)) {
    support_ranked$adduct_support <- 0L
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

  support_ranked <- support_ranked |>
    tidytable::mutate(
      has_structure_match = !is.na(candidate_structure_error_mz),
      has_pairwise_support = source %in%
        c("pair", "preassigned", "preassigned_propagated"),
      has_modifier_pairwise_support = source %in% c("cluster", "loss"),
      has_multicharge_evidence = source %in% c("multi"),
      has_evidence_support = source %in% c("evidence"),
      has_baseline_fallback = baseline_active &
        !is.na(adduct) &
        adduct == baseline_label,
      coverage_class = tidytable::case_when(
        has_structure_match ~ "structure_matched",
        has_pairwise_support ~ "pairwise_supported",
        has_multicharge_evidence ~ "evidence_multicharge_supported",
        has_modifier_pairwise_support ~ "modifier_pairwise_supported",
        has_evidence_support ~ "evidence_supported",
        has_baseline_fallback ~ "baseline_fallback",
        TRUE ~ "adduct_only"
      ),
      coverage_tier = tidytable::case_when(
        coverage_class == "structure_matched" ~ 1L,
        coverage_class == "pairwise_supported" ~ 2L,
        coverage_class == "evidence_multicharge_supported" ~ 3L,
        coverage_class == "modifier_pairwise_supported" ~ 4L,
        coverage_class == "evidence_supported" ~ 5L,
        coverage_class == "baseline_fallback" ~ 6L,
        TRUE ~ 7L
      )
    )

  feature_annotation_counts <- support_ranked |>
    tidytable::summarize(N_annotations = .N, .by = feature_id)

  best_feature_class <- support_ranked |>
    tidytable::arrange(
      feature_id,
      coverage_tier,
      tidytable::desc(has_structure_match),
      tidytable::desc(has_pairwise_support),
      tidytable::desc(has_multicharge_evidence),
      tidytable::desc(has_modifier_pairwise_support),
      tidytable::desc(has_evidence_support),
      tidytable::desc(has_baseline_fallback),
      tidytable::desc(adduct_support),
      adduct
    )
  best_feature_class <- best_feature_class |>
    tidytable::slice_head(n = 1, .by = feature_id)

  best_summary <- best_feature_class |>
    tidytable::left_join(feature_annotation_counts, by = "feature_id") |>
    tidytable::summarize(
      N_features = .N,
      N_annotations = sum(N_annotations, na.rm = TRUE),
      .by = c(coverage_class, coverage_tier)
    )

  any_summary <- support_ranked |>
    tidytable::summarize(
      N_features = tidytable::n_distinct(feature_id),
      N_annotations = .N,
      .by = c(coverage_class, coverage_tier)
    )

  all_features <- tidytable::n_distinct(support_ranked$feature_id)
  all_annotations <- nrow(support_ranked)

  all_rows <- tidytable::tidytable(
    coverage_scope = c("best", "any"),
    coverage_class = c("all", "all"),
    coverage_tier = c(0L, 0L),
    N_features = c(all_features, all_features),
    N_annotations = c(all_annotations, all_annotations)
  )

  coverage_report <- tidytable::bind_rows(
    all_rows,
    tidytable::as_tidytable(best_summary) |>
      tidytable::mutate(coverage_scope = "best"),
    tidytable::as_tidytable(any_summary) |>
      tidytable::mutate(coverage_scope = "any")
  )

  coverage_report$Pct_features <- sprintf(
    "%.2f%%",
    if (all_features == 0L) {
      rep(0, nrow(coverage_report))
    } else {
      100 * coverage_report$N_features / all_features
    }
  )
  coverage_report$Pct_annotations <- sprintf(
    "%.2f%%",
    if (all_annotations == 0L) {
      rep(0, nrow(coverage_report))
    } else {
      100 * coverage_report$N_annotations / all_annotations
    }
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

  rm(
    support_ranked,
    feature_annotation_counts,
    best_feature_class,
    best_summary,
    any_summary,
    all_rows
  )

  coverage_report
}
