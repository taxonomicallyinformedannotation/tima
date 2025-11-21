# Test Suite: change_params_small ----

library(testthat)

# These tests are skipped because change_params_small() requires:
# 1. go_to_cache() which changes working directory
# 2. copy_backbone() to set up directory structure
# 3. Modifies global state and creates files in cache

# Instead, we test the function signature and basic properties

test_that("change_params_small function exists and is callable", {
  expect_true(exists("change_params_small"))
  expect_type(change_params_small, "closure")
})

test_that("change_params_small has correct parameter names", {
  params <- names(formals(change_params_small))
  expect_true("fil_pat" %in% params)
  expect_true("fil_fea_raw" %in% params)
  expect_true("fil_met_raw" %in% params)
  expect_true("fil_sir_raw" %in% params)
  expect_true("fil_spe_raw" %in% params)
  expect_true("ms_pol" %in% params)
  expect_true("org_tax" %in% params)
  expect_true("hig_con" %in% params)
  expect_true("summarize" %in% params)
})

test_that("change_params_small all parameters have NULL defaults", {
  params <- formals(change_params_small)
  expect_null(params$fil_pat)
  expect_null(params$fil_fea_raw)
  expect_null(params$fil_met_raw)
  expect_null(params$fil_sir_raw)
  expect_null(params$fil_spe_raw)
  expect_null(params$ms_pol)
  expect_null(params$org_tax)
  expect_null(params$hig_con)
  expect_null(params$summarize)
})

# Note: Functional tests would require integration testing with:
# - copy_backbone() to create directory structure
# - Proper cache setup
# - YAML file validation
# These are better suited for integration tests rather than unit tests
