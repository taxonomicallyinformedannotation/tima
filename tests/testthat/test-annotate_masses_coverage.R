# Test Suite: annotate_masses_coverage ----

library(testthat)

test_that("resolve_annotate_masses_coverage_mode applies defaults and fallback", {
  expect_identical(
    resolve_annotate_masses_coverage_mode(NULL),
    "best_supported_conflict_free"
  )
  expect_identical(
    resolve_annotate_masses_coverage_mode(NA_character_),
    "best_supported_conflict_free"
  )
  expect_identical(
    resolve_annotate_masses_coverage_mode("broad_conflict_free"),
    "broad_conflict_free"
  )
  expect_identical(
    resolve_annotate_masses_coverage_mode("unknown_mode"),
    "best_supported_conflict_free"
  )
})


test_that("derive_annotate_masses_coverage_path handles common path shapes", {
  expect_identical(
    derive_annotate_masses_coverage_path("results/ann.tsv"),
    "results/ann_coverage.tsv"
  )
  expect_identical(
    derive_annotate_masses_coverage_path("results/ann.tsv.gz"),
    "results/ann_coverage.tsv.gz"
  )
  expect_identical(
    derive_annotate_masses_coverage_path("results/ann"),
    "results/ann_coverage.tsv"
  )
  expect_identical(
    derive_annotate_masses_coverage_path(NA_character_),
    "annotate_masses_coverage.tsv"
  )
})

test_that("build_annotate_masses_coverage_report returns zero rows summary for empty annotations", {
  report <- build_annotate_masses_coverage_report(
    annotations = tidytable::tidytable(),
    baseline_adduct = NA_character_
  )

  expect_s3_class(report, "data.frame")
  expect_identical(report$coverage_scope, c("best", "any"))
  expect_identical(report$coverage_class, c("all", "all"))
  expect_identical(report$coverage_tier, c(0L, 0L))
  expect_identical(report$N_features, c(0L, 0L))
  expect_identical(report$N_annotations, c(0L, 0L))
})

test_that("build_annotate_masses_coverage_report classifies tiers and baseline fallback", {
  annotations <- tidytable::tidytable(
    feature_id = paste0("f", 1:7),
    source = c(
      "pair",
      "pair",
      "multi",
      "cluster",
      "evidence",
      "other",
      "other"
    ),
    adduct = c(
      "[M+H]+",
      "[M+H]+",
      "[M+H]+",
      "[M+H]+",
      "[M+H]+",
      "[M+H]+",
      "[M+Na]+"
    ),
    candidate_structure_error_mz = c(0.01, NA, NA, NA, NA, NA, NA),
    adduct_support = c(5L, 4L, 3L, 2L, 1L, 1L, 1L)
  )

  report <- build_annotate_masses_coverage_report(
    annotations = annotations,
    baseline_adduct = "[M+H]+"
  )

  best_rows <- report[report$coverage_scope == "best", ]
  expect_true(all(
    c(
      "structure_matched",
      "pairwise_supported",
      "evidence_multicharge_supported",
      "modifier_pairwise_supported",
      "evidence_supported",
      "baseline_fallback",
      "adduct_only"
    ) %in%
      best_rows$coverage_class
  ))

  all_best <- best_rows[best_rows$coverage_class == "all", ]
  expect_identical(all_best$N_features[[1]], 7L)
  expect_identical(all_best$N_annotations[[1]], 7L)
})

test_that("build_annotate_masses_coverage_report tolerates missing optional columns", {
  annotations <- tidytable::tidytable(feature_id = c("x1", "x2"))

  report <- build_annotate_masses_coverage_report(
    annotations = annotations,
    baseline_adduct = NULL
  )

  adduct_only_best <- report[
    report$coverage_scope == "best" & report$coverage_class == "adduct_only",
  ]
  expect_identical(adduct_only_best$N_features[[1]], 2L)
  expect_identical(adduct_only_best$N_annotations[[1]], 2L)
})

test_that("write_empty_annotate_masses_outputs exports expected files", {
  calls <- new.env(parent = emptyenv())
  calls$list <- list()

  outputs <- with_mocked_bindings(
    export_output = function(x, file) {
      calls$list[[length(calls$list) + 1L]] <- list(
        file = as.character(file[[1L]]),
        nrow = nrow(x),
        ncol = ncol(x)
      )
      invisible(NULL)
    },
    write_empty_annotate_masses_outputs(
      output_annotations = "results/annotate_masses.tsv",
      output_edges = "results/edges.tsv"
    )
  )

  expect_identical(outputs[["annotations"]], "results/annotate_masses.tsv")
  expect_identical(outputs[["edges"]], "results/edges.tsv")
  expect_length(calls$list, 3L)

  exported_files <- vapply(calls$list, `[[`, character(1), "file")
  expect_true("results/annotate_masses.tsv" %in% exported_files)
  expect_true("results/edges.tsv" %in% exported_files)
  expect_true("results/annotate_masses_coverage.tsv" %in% exported_files)
})

test_that("build_annotate_masses_coverage_report distinguishes best vs any scope on multi-row features", {
  # f1 has both a structure_matched row (tier 1) and a baseline_fallback row (tier 6);
  # f2 has only baseline_fallback (tier 6).
  annotations <- tidytable::tidytable(
    feature_id = c("f1", "f1", "f2"),
    source = c("pair", "baseline", "baseline"),
    adduct = c("[M+Na]+", "[M+H]+", "[M+H]+"),
    candidate_structure_error_mz = c(0.001, NA_real_, NA_real_),
    adduct_support = c(2L, 0L, 0L)
  )

  report <- build_annotate_masses_coverage_report(
    annotations = annotations,
    baseline_adduct = "[M+H]+"
  )

  best_str <- report[
    report$coverage_scope == "best" &
      report$coverage_class == "structure_matched",
  ]
  best_base <- report[
    report$coverage_scope == "best" &
      report$coverage_class == "baseline_fallback",
  ]
  any_base <- report[
    report$coverage_scope == "any" &
      report$coverage_class == "baseline_fallback",
  ]

  # In "best" scope, f1's 2 annotations belong to tier 1 (structure_matched),
  # and only f2 (1 annotation) belongs to tier 6 (baseline_fallback).
  expect_identical(best_str$N_features[[1L]], 1L)
  expect_identical(best_str$N_annotations[[1L]], 2L)
  expect_identical(best_base$N_features[[1L]], 1L)
  expect_identical(best_base$N_annotations[[1L]], 1L)

  # In "any" scope, both f1 and f2 have a baseline_fallback annotation (2 features, 2 rows).
  expect_identical(any_base$N_features[[1L]], 2L)
  expect_identical(any_base$N_annotations[[1L]], 2L)
})

test_that("coverage_audit attribute survives post-matching annotation transforms", {
  ann <- tidytable::tidytable(
    feature_id = c("f1", "f1", "f2"),
    mz = c(100, 100, 200),
    rt = c(1, 1, 2),
    adduct = c("[M+H]+", "[M+Na]+", "[M+H]+"),
    mass = c(99, 77, 199),
    source = c("pair", "baseline", "baseline"),
    candidate_adduct_origin = c("supported", "baseline", "baseline"),
    adduct_support = c(2L, 0L, 0L),
    structure_exact_mass = c(99.0001, NA_real_, NA_real_),
    error_mz = c(0.0001, NA_real_, NA_real_),
    candidate_structure_error_mz = c(0.0001, NA_real_, NA_real_)
  )
  edges <- tidytable::tidytable(
    feature_id = "f1",
    adduct = "[M+H]+",
    feature_id_dest = "f2",
    adduct_dest = "[M+H]+"
  )

  enforced <- enforce_non_conflicting_annotation_states(
    annotations = ann,
    adduct_edges = edges,
    baseline_adduct = "[M+H]+"
  )
  audit <- attr(enforced, "coverage_audit")
  expect_type(audit, "list")
  expect_identical(audit$n_input, 3L)
  expect_identical(audit$n_kept, 2L)
  expect_identical(audit$n_dropped, 1L)

  derived <- derive_primary_secondary_annotations(enforced, "[M+H]+")
  expect_identical(attr(derived, "coverage_audit"), audit)

  retained <- retain_supported_single_m_edges(
    adduct_edges = edges,
    cluster_edges = tidytable::tidytable(
      feature_id = character(),
      adduct = character(),
      feature_id_dest = character(),
      adduct_dest = character()
    ),
    loss_edges = tidytable::tidytable(
      feature_id = character(),
      loss = character(),
      feature_id_dest = character()
    ),
    annotations = derived,
    tolerance_ppm = 10,
    tolerance_dalton = 0.0025
  )
  expect_identical(attr(retained$annotations, "coverage_audit"), audit)
  expect_true(all(retained$annotations$component_id == "M_1"))
})
