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
    "doi must be a single non-empty character string"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = 123,
      pattern = "file.txt",
      path = "output.txt"
    ),
    "doi must be a single non-empty character string"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = NULL,
      pattern = "file.txt",
      path = "output.txt"
    ),
    "doi must be a single non-empty character string"
  )
})

test_that("test-get_last_version_from_zenodo validates pattern parameter", {
  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.123",
      pattern = c("file1.txt", "file2.txt"),
      path = "output.txt"
    ),
    "pattern must be a single non-empty character string"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.123",
      pattern = 123,
      path = "output.txt"
    ),
    "pattern must be a single non-empty character string"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.123",
      pattern = NULL,
      path = "output.txt"
    ),
    "pattern must be a single non-empty character string"
  )
})

test_that("test-get_last_version_from_zenodo validates path parameter", {
  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.123",
      pattern = "file.txt",
      path = c("output1.txt", "output2.txt")
    ),
    "path must be a single non-empty character string"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.123",
      pattern = "file.txt",
      path = 123
    ),
    "path must be a single non-empty character string"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.123",
      pattern = "file.txt",
      path = NULL
    ),
    "path must be a single non-empty character string"
  )
})

test_that("test-get_last_version_from_zenodo requires all parameters", {
  expect_error(
    get_last_version_from_zenodo(
      pattern = "file.txt",
      path = "output.txt"
    ),
    "doi must be a single non-empty character string"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.123",
      path = "output.txt"
    ),
    "pattern must be a single non-empty character string"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.123",
      pattern = "file.txt"
    ),
    "path must be a single non-empty character string"
  )
})

## Behavior ----

test_that("test-get_last_version_from_zenodo handles invalid DOI gracefully", {
  skip_on_cran()
  skip_if_offline()

  output <- tempfile(fileext = ".txt")

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.999999999",
      pattern = "*.txt",
      path = output
    ),
    "Failed to retrieve Zenodo record"
  )
})

# Integration Tests: Testing through main function ----

test_that("get_last_version_from_zenodo validates all inputs", {
  expect_error(
    get_last_version_from_zenodo(
      doi = "invalid",
      pattern = "test",
      path = "out"
    ),
    "Invalid Zenodo DOI format"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.123",
      pattern = 123,
      path = "out"
    ),
    "pattern must be"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.123",
      pattern = "test",
      path = c("a", "b")
    ),
    "path must be"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = "",
      pattern = "test",
      path = "out"
    ),
    "must be a single non-empty character string"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.123",
      pattern = "",
      path = "out"
    ),
    "must be a single non-empty character string"
  )
})

test_that("get_last_version_from_zenodo validates DOI format", {
  expect_error(
    get_last_version_from_zenodo(
      doi = "invalid-doi",
      pattern = "data.csv",
      path = "output/data.csv"
    ),
    "Invalid Zenodo DOI format"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.1234/zenodo.123",
      pattern = "data.csv",
      path = "output/data.csv"
    ),
    "Please verify the DOI is correct"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = "zenodo.123456",
      pattern = "data.csv",
      path = "output/data.csv"
    ),
    "Invalid Zenodo DOI format"
  )
})

test_that("get_last_version_from_zenodo rejects invalid types", {
  expect_error(
    get_last_version_from_zenodo(
      doi = 123,
      pattern = "data.csv",
      path = "output/data.csv"
    ),
    "doi must be a single non-empty character string"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = c("10.5281/zenodo.123", "10.5281/zenodo.456"),
      pattern = "data.csv",
      path = "output/data.csv"
    ),
    "doi must be a single non-empty character string"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.1234567",
      pattern = 123,
      path = "output/data.csv"
    ),
    "pattern must be a single non-empty character string"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.1234567",
      pattern = c("a", "b"),
      path = "output/data.csv"
    ),
    "pattern must be a single non-empty character string"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.1234567",
      pattern = "data.csv",
      path = 123
    ),
    "path must be a single non-empty character string"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.1234567",
      pattern = "data.csv",
      path = c("a", "b")
    ),
    "path must be a single non-empty character string"
  )
})

test_that("get_last_version_from_zenodo handles network errors gracefully", {
  skip_if_offline()
  skip_on_cran()

  # Use invalid record that doesn't exist
  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.9999999999",
      pattern = "test",
      path = temp_test_path("test.txt")
    ),
    "Zenodo record not found|Failed to retrieve"
  )
})

test_that("get_last_version_from_zenodo handles file size comparison", {
  skip_if_offline()
  skip_on_cran()
  skip("Requires actual Zenodo download - use for integration testing")

  # This would test actual download from a real Zenodo record
  # Skipped to avoid network calls in regular testing
  path <- temp_test_path("lotus_test.csv.gz")

  result <- get_last_version_from_zenodo(
    doi = "10.5281/zenodo.5794106",
    pattern = "lotus.csv.gz",
    path = path
  )

  expect_equal(result, path)
  expect_true(file.exists(path))

  # Second call should skip download
  result2 <- get_last_version_from_zenodo(
    doi = "10.5281/zenodo.5794106",
    pattern = "lotus.csv.gz",
    path = path
  )

  expect_equal(result2, path)
})

# Edge Cases ----

test_that("get_last_version_from_zenodo accepts various valid DOI formats", {
  skip_if_offline()
  skip_on_cran()
  skip("Requires network access")

  # Standard Zenodo DOI - should not error during validation
  # (will error at network layer which we catch differently)
  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.1234567",
      pattern = "test",
      path = temp_test_path("out")
    ),
    "Failed to retrieve|not found" # Network error, not validation error
  )

  # Long record ID
  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.123456789",
      pattern = "test",
      path = temp_test_path("out")
    ),
    "Failed to retrieve|not found" # Network error, not validation error
  )
})

# Regression Tests ----

test_that("get_last_version_from_zenodo maintains backward compatibility", {
  # Ensure function signature hasn't changed
  params <- names(formals(get_last_version_from_zenodo))

  expect_equal(length(params), 3)
  expect_true("doi" %in% params)
  expect_true("pattern" %in% params)
  expect_true("path" %in% params)
})

test_that("get_last_version_from_zenodo returns path invisibly", {
  skip_if_offline()
  skip_on_cran()
  skip("Requires actual Zenodo download")

  path <- temp_test_path("test_return.txt")

  # Test that function returns path (invisibly or not)
  result <- get_last_version_from_zenodo(
    doi = "10.5281/zenodo.5794106",
    pattern = "lotus",
    path = path
  )

  expect_type(result, "character")
  expect_equal(result, path)
})

# Error Message Quality Tests ----

test_that("error messages are informative", {
  # DOI format error includes expected format
  expect_error(
    get_last_version_from_zenodo(
      doi = "bad-doi",
      pattern = "test",
      path = "out"
    ),
    "Expected format.*10\\.5281/zenodo"
  )

  # Empty string errors are clear
  expect_error(
    get_last_version_from_zenodo(
      doi = "",
      pattern = "test",
      path = "out"
    ),
    "non-empty"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.123",
      pattern = "",
      path = "out"
    ),
    "non-empty"
  )
})

# Parameter Validation Order ----

test_that("get_last_version_from_zenodo validates parameters in correct order", {
  # DOI should be validated first
  expect_error(
    get_last_version_from_zenodo(
      doi = 123,
      pattern = 456,
      path = 789
    ),
    "doi must be"
  )

  # Then pattern
  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.123",
      pattern = 456,
      path = 789
    ),
    "pattern must be"
  )

  # Then path
  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.123",
      pattern = "test",
      path = 789
    ),
    "path must be"
  )
})

# Real tests ----

test_that("get_last_version_from_zenodo success", {
  tmp <- temp_test_dir("get_file_dir_create")
  withr::local_dir(tmp)
  expect_no_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.7534071",
      pattern = "230106_frozen.csv.gz",
      path = file.path(tmp, "230106_frozen.csv.gz")
    )
  )
  expect_no_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.7534071",
      pattern = "230106_frozen.csv.gz",
      path = file.path(tmp, "230106_frozen.csv.gz")
    )
  )
})
