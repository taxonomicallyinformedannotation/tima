# Test Suite: get_last_version_from_zenodo ----

library(testthat)

## Validation ----

test_that("test-get_last_version_from_zenodo validates doi parameter", {
  expect_error(
    get_last_version_from_zenodo(
      doi = c("10.5281/zenodo.123", "10.5281/zenodo.456"),
      pattern = "file.txt",
      path = "output.txt"
    ),
    "doi must be a single character string"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = 123,
      pattern = "file.txt",
      path = "output.txt"
    ),
    "doi must be a single character string"
  )
})

test_that("test-get_last_version_from_zenodo validates pattern parameter", {
  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.123",
      pattern = c("file1.txt", "file2.txt"),
      path = "output.txt"
    ),
    "pattern must be a single character string"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.123",
      pattern = 123,
      path = "output.txt"
    ),
    "pattern must be a single character string"
  )
})

test_that("test-get_last_version_from_zenodo validates path parameter", {
  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.123",
      pattern = "file.txt",
      path = c("output1.txt", "output2.txt")
    ),
    "path must be a single character string"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.123",
      pattern = "file.txt",
      path = 123
    ),
    "path must be a single character string"
  )
})

test_that("test-get_last_version_from_zenodo requires all parameters", {
  expect_error(
    get_last_version_from_zenodo(
      pattern = "file.txt",
      path = "output.txt"
    ),
    "doi must be a single character string"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.123",
      path = "output.txt"
    ),
    "pattern must be a single character string"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.123",
      pattern = "file.txt"
    ),
    "path must be a single character string"
  )
})

## Behavior ----

test_that("test-get_last_version_from_zenodo handles invalid DOI gracefully", {
  output <- file.path("output.txt")

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.999999999",
      pattern = "*.txt",
      path = output
    ),
    "Failed to retrieve Zenodo record"
  )
})
