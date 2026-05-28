# Test Suite: get_default_paths ----

library(testthat)

test_that("get_default_paths returns named list", {
  paths <- get_default_paths()
  expect_type(paths, "list")
  expect_true(length(paths) > 0)
})

test_that("get_default_paths errors for missing yaml", {
  expect_error(
    get_default_paths(yaml = "nonexistent.yaml"),
    "not found",
    class = "tima_validation_error"
  )
})

test_that("get_default_paths errors for malformed yaml", {
  yml <- temp_test_path("broken_paths.yaml")
  writeLines("data: [", yml)

  expect_error(
    get_default_paths(yaml = yml),
    class = "tima_runtime_error"
  )
})

test_that("get_default_paths honors interim params override option", {
  yml <- temp_test_path("paths_override.yaml")
  writeLines(
    c(
      "data:",
      "  interim:",
      "    params:",
      "      path: old/path"
    ),
    yml
  )

  withr::local_options(tima.test.interim_params_dir = "new/path")
  paths <- get_default_paths(yaml = yml)

  expect_identical(paths$data$interim$params$path, "new/path")
})

