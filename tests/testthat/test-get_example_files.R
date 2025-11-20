# Test Suite: get_example_files ----

library(testthat)

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
  expect_no_error(
    get_example_files(example = "features")
  )
})
