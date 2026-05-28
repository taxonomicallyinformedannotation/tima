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

test_that("resolve_adduct_consistency_config normalizes invalid values", {
  cfg <- resolve_adduct_consistency_config(
    adduct_consistency = NULL,
    adduct_min_support = NULL,
    adduct_consistency_min_degree = NULL
  )
  expect_identical(cfg$adduct_consistency, "conditional")
  expect_identical(cfg$adduct_min_support, 2L)
  expect_identical(cfg$adduct_consistency_min_degree, 3L)

  cfg_invalid <- resolve_adduct_consistency_config(
    adduct_consistency = "bad",
    adduct_min_support = "not-int",
    adduct_consistency_min_degree = 0
  )
  expect_identical(cfg_invalid$adduct_consistency, "conditional")
  expect_identical(cfg_invalid$adduct_min_support, 2L)
  expect_identical(cfg_invalid$adduct_consistency_min_degree, 3L)
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
  calls <- list()

  outputs <- with_mocked_bindings(
    export_output = function(x, file) {
      calls[[length(calls) + 1L]] <<- list(
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
  expect_length(calls, 3L)

  exported_files <- vapply(calls, `[[`, character(1), "file")
  expect_true("results/annotate_masses.tsv" %in% exported_files)
  expect_true("results/edges.tsv" %in% exported_files)
  expect_true("results/annotate_masses_coverage.tsv" %in% exported_files)
})
