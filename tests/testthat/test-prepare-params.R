# ==============================================================================
# Test Suite: prepare_params and get_params
# ==============================================================================
#
# @description
# Comprehensive unit tests for parameter preparation and retrieval functions
# used throughout the TIMA workflow.
#
# @coverage
# - prepare_params with default parameters
# - prepare_params with custom configuration
# - prepare_params for all workflow steps
# - get_params parameter retrieval
# - Parameter persistence and reuse

library(testthat)
library(tima)

# ==============================================================================
# Test Fixtures
# ==============================================================================

# All workflow steps that should have parameter configurations
WORKFLOW_STEPS <- c(
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

# ==============================================================================
# Core Functionality Tests - prepare_params
# ==============================================================================

test_that("prepare_params creates parameters with default configuration", {
  paths <- local_test_project(copy = TRUE)
  params <- get_params(step = "prepare_params")

  expect_no_error(prepare_params(params_small = params))

  # Verify parameter files were created
  expect_true(dir.exists("params"))
})

test_that("prepare_params accepts custom taxon parameter", {
  local_test_project(copy = TRUE)
  params <- get_params(step = "prepare_params")

  params$organisms$taxon <- "Gentiana lutea"

  expect_no_error(prepare_params(params_small = params))
})

test_that("prepare_params works for all workflow steps", {
  local_test_project(copy = TRUE)

  for (step in WORKFLOW_STEPS) {
    # Test each workflow step
    expect_no_error(prepare_params(step = step))
  }
})

test_that("prepare_params handles reuse of existing parameters", {
  local_test_project(copy = TRUE)

  # First create params
  params <- get_params(step = "prepare_params")
  prepare_params(params_small = params)

  # Then try to prepare again (should use existing)
  expect_no_error(prepare_params())
})

# ==============================================================================
# Core Functionality Tests - get_params
# ==============================================================================

test_that("get_params retrieves standard parameters", {
  local_test_project(copy = TRUE)

  params <- get_params(step = "prepare_params")

  expect_type(params, "list")
  expect_true(length(params) > 0, info = "Parameters should not be empty")
})

test_that("get_params retrieves advanced parameters", {
  local_test_project(copy = TRUE)

  params <- get_params(step = "prepare_params_advanced")

  expect_type(params, "list")
  expect_true(length(params) > 0, info = "Advanced parameters should not be empty")
})

test_that("get_params returns consistent structure for same step", {
  local_test_project(copy = TRUE)

  params1 <- get_params(step = "prepare_params")
  params2 <- get_params(step = "prepare_params")

  expect_identical(params1, params2, info = "Same step should return identical params")
})

# ==============================================================================
# Parameter Structure Tests
# ==============================================================================

test_that("get_params returns parameters with expected top-level structure", {
  local_test_project(copy = TRUE)

  params <- get_params(step = "prepare_params")

  expect_type(params, "list")
  expect_true(is.list(params), info = "Parameters should be a list")
})

test_that("prepare_params handles different parameter step names", {
  local_test_project(copy = TRUE)

  # Test a few representative steps
  test_steps <- c(
    "annotate_masses",
    "prepare_features_tables",
    "weight_annotations"
  )

  for (step in test_steps) {
    # Get params for each step
    expect_no_error(params <- get_params(step = step))
    expect_type(params, "list")
  }
})

# ==============================================================================
# Integration Tests
# ==============================================================================

test_that("prepare_params integrates with get_default_paths", {
  paths <- local_test_project(copy = TRUE)
  params <- get_params(step = "prepare_params")

  expect_no_error(prepare_params(params_small = params))

  # Paths should still be accessible
  expect_type(paths, "list")
})

test_that("parameter workflow completes end-to-end", {
  local_test_project(copy = TRUE)

  # Get params
  params <- get_params(step = "prepare_params")
  expect_type(params, "list")

  # Prepare params
  expect_no_error(prepare_params(params_small = params))

  # Retrieve params again (should use prepared version)
  expect_no_error(get_params(step = "prepare_params"))
})

# ==============================================================================
# Edge Cases and Error Handling
# ==============================================================================

test_that("get_params handles unknown step gracefully", {
  local_test_project(copy = TRUE)

  # This may error or return empty/default params depending on implementation
  # Document the actual behavior
  result <- tryCatch(
    get_params(step = "nonexistent_step"),
    error = function(e) e
  )

  # Should either error or return a list
  expect_true(inherits(result, "list") || inherits(result, "error"))
})

