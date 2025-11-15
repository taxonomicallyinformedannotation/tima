# ==============================================================================
# Test Suite: load_yaml_files
# ==============================================================================
library(testthat)
library(tima)

test_that("load_yaml_files returns list", {
  result <- load_yaml_files()
  expect_type(result, "list")
})

test_that("load_yaml_files includes prepare params entries", {
  loaded <- load_yaml_files()
  expect_true(all(
    c("prepare_params", "prepare_params_advanced") %in% loaded$yaml_names
  ))
})
