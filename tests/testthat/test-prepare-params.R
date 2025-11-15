# Test: Parameter Preparation Functions
library(testthat)

test_that("prepare_params works with default parameters", {
  paths <- local_test_project(copy = TRUE)
  params <- get_params(step = "prepare_params")

  expect_no_error(prepare_params(params_small = params))
})

test_that("prepare_params works with custom taxon", {
  local_test_project(copy = TRUE)
  params <- get_params(step = "prepare_params")

  params$organisms$taxon <- "Gentiana lutea"
  expect_no_error(prepare_params(params_small = params))
})

test_that("prepare_params works for all workflow steps", {
  local_test_project(copy = TRUE)

  steps <- c(
    "annotate_masses",
    "annotate_spectra",
    "create_components",
    "create_edges_spectra",
    "filter_annotations",
    "prepare_annotations_gnps",
    "prepare_annotations_sirius",
    "prepare_annotations_spectra",
    "prepare_features_components",
    "prepare_features_edges",
    "prepare_features_tables",
    "prepare_libraries_rt",
    "prepare_libraries_sop_closed",
    "prepare_libraries_sop_ecmdb",
    "prepare_libraries_sop_hmdb",
    "prepare_libraries_sop_lotus",
    "prepare_libraries_sop_merged",
    "prepare_libraries_spectra",
    "prepare_taxa",
    "weight_annotations"
  )

  for (step in steps) {
    expect_no_error(
      prepare_params(step = step),
    )
  }
})

test_that("prepare_params works with existing parameters", {
  local_test_project(copy = TRUE)

  # First create params
  params <- get_params(step = "prepare_params")
  prepare_params(params_small = params)

  # Then try to prepare again (should use existing)
  expect_no_error(prepare_params())
})

test_that("get_params retrieves advanced parameters", {
  local_test_project(copy = TRUE)

  params <- get_params(step = "prepare_params_advanced")
  expect_type(params, "list")
  expect_true(length(params) > 0)
})

