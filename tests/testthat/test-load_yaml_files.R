# ==============================================================================
# Test Suite: load_yaml_files
# ==============================================================================
library(testthat)
library(tima)

test_that("load_yaml_files returns list", {
  result <- load_yaml_files()
  expect_type(result, "list")
})

