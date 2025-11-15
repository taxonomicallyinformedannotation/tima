# ==============================================================================
# Test Suite: get_default_paths
# ==============================================================================
library(testthat)
library(tima)

test_that("get_default_paths returns named list", {
  paths <- get_default_paths()
  expect_type(paths, "list")
  expect_true(length(paths) > 0)
})

test_that("get_default_paths errors for missing yaml", {
  expect_error(get_default_paths(yaml = "nonexistent.yaml"), "not found")
})
