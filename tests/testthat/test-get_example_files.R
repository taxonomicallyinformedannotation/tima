# Test Suite: get_example_files ----

library(testthat)

test_that("get_example_files retrieves specific example files", {
  withr::local_dir(temp_test_dir("get_example_files_specific"))
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
  withr::local_dir(temp_test_dir("get_example_files_default"))
  expect_no_error(get_example_files())
})

test_that("get_example_files handles cache mode correctly", {
  withr::local_dir(temp_test_dir("get_example_files_cache"))
  paths <- local_test_project(copy = TRUE)

  expect_no_error(
    get_example_files(
      example = c("features"),
      in_cache = FALSE
    )
  )

  expect_no_error(
    get_example_files(
      example = c("features"),
      in_cache = TRUE
    )
  )
})

test_that("get_example_files handles single example file", {
  withr::local_dir(temp_test_dir("get_example_files_single"))
  expect_no_error(
    get_example_files(example = "features")
  )
})
