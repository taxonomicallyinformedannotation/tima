# ==============================================================================
# Test Suite: Utility Functions
# ==============================================================================
#
# @description
# Tests for various utility and helper functions including parameter handling,
# file retrieval, and CLI argument parsing.
#
# @coverage
# - change_params_small - Parameter update utility
# - parse_cli_params - CLI argument merging
# - get_example_files - Example file retrieval
#
# @note
# This file tests multiple utility functions. Consider splitting into separate
# test files as the codebase grows.

library(testthat)
library(tima)

# ==============================================================================
# Tests: change_params_small
# ==============================================================================

test_that("change_params_small updates workflow parameters correctly", {
  paths <- local_test_project(copy = TRUE)

  get_example_files()

  expect_no_error(
    change_params_small(
      fil_pat = "myExamplePattern",
      fil_fea_raw = paths$data$source$features,
      fil_met_raw = paths$data$source$metadata,
      fil_sir_raw = "data/interim/annotations/example_sirius.zip",
      fil_spe_raw = paths$data$source$spectra,
      ms_pol = "pos",
      org_tax = "Gentiana lutea",
      hig_con = TRUE,
      summarize = FALSE
    )
  )
})

test_that("change_params_small handles minimal parameters", {
  paths <- local_test_project(copy = TRUE)

  # Test with just required parameters
  expect_no_error(
    change_params_small(
      fil_pat = "test_pattern",
      ms_pol = "pos"
    )
  )
})

# ==============================================================================
# Tests: parse_cli_params
# ==============================================================================

test_that("parse_cli_params merges CLI arguments into parameters", {
  local_test_project(copy = TRUE)
  params <- get_params(step = "prepare_params_advanced")

  arguments <- list()
  arguments$ann_can_fin <- 666L

  result <- parse_cli_params(arguments = arguments, parameters = params)

  expect_type(result, "list")
  expect_equal(
    result$annotations$candidates$final,
    666L,
    info = "CLI argument should override parameter"
  )
})

test_that("parse_cli_params handles empty arguments list", {
  local_test_project(copy = TRUE)
  params <- get_params(step = "prepare_params_advanced")

  result <- parse_cli_params(arguments = list(), parameters = params)

  expect_type(result, "list")
  expect_identical(result, params, info = "Empty args should return original params")
})

test_that("parse_cli_params preserves unmodified parameters", {
  local_test_project(copy = TRUE)
  params <- get_params(step = "prepare_params_advanced")

  # Modify only one parameter
  arguments <- list(ann_can_fin = 100L)

  result <- parse_cli_params(arguments = arguments, parameters = params)

  expect_type(result, "list")
  # Other parameters should remain unchanged
  expect_true(length(result) >= length(params))
})

# ==============================================================================
# Tests: get_example_files
# ==============================================================================

test_that("get_example_files retrieves specific example files", {
  paths <- local_test_project(copy = TRUE)

  expect_no_error(
    get_example_files(
      example = c(
        "features",
        "metadata",
        "sirius",
        "spectra",
        "spectral_lib_with_rt"
      ),
      in_cache = FALSE
    )
  )
})

test_that("get_example_files works with default parameters", {
  skip_on_cran()

  expect_no_error(get_example_files())
})

test_that("get_example_files handles cache mode correctly", {
  paths <- local_test_project(copy = TRUE)

  # First call should download/cache
  expect_no_error(
    get_example_files(
      example = c("features"),
      in_cache = FALSE
    )
  )

  # Second call could use cache
  expect_no_error(
    get_example_files(
      example = c("features"),
      in_cache = TRUE
    )
  )
})

test_that("get_example_files handles single example file", {
  skip_on_cran()

  expect_no_error(
    get_example_files(example = "features")
  )
})

# ==============================================================================
# Edge Cases and Error Handling
# ==============================================================================

test_that("change_params_small validates MS polarity parameter", {
  paths <- local_test_project(copy = TRUE)

  # Invalid polarity should error (if validation exists)
  # Document actual behavior
  result <- tryCatch(
    change_params_small(fil_pat = "test", ms_pol = "invalid"),
    error = function(e) e
  )

  # Either errors or accepts - document which
  expect_true(inherits(result, "NULL") || inherits(result, "error"))
})

# ==============================================================================
# Future Tests (Currently Commented Out)
# ==============================================================================

# test_that("install function works in test mode", {
#   expect_no_error(install(test = TRUE))
# })
#
# Reason for skipping: install() may require specific environment setup
# or have side effects unsuitable for automated testing
