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
  withr::local_options(list(
    tima.zenodo.max_attempts = 1L,
    tima.zenodo.req_retry_max_tries = 1L,
    tima.zenodo.backoff_cap_s = 0
  ))

  output <- tempfile(fileext = ".txt")

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.999999999",
      pattern = "*.txt",
      path = output,
      timeout_s = 1
    ),
    "failed to retrieve Zenodo record|Zenodo record not found",
    class = "tima_error"
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
    "invalid Zenodo DOI format",
    class = "tima_validation_error"
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
  withr::local_options(list(
    tima.zenodo.max_attempts = 1L,
    tima.zenodo.req_retry_max_tries = 1L,
    tima.zenodo.backoff_cap_s = 0
  ))

  expect_error(
    get_last_version_from_zenodo(
      doi = "invalid-doi",
      pattern = "data.csv",
      path = "output/data.csv"
    ),
    "invalid Zenodo DOI format",
    class = "tima_validation_error"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.1234/zenodo.123",
      pattern = "data.csv",
      path = "output/data.csv",
      timeout_s = 1
    ),
    "invalid Zenodo DOI format|failed to retrieve Zenodo record|Zenodo record not found",
    class = "tima_error"
  )

  expect_error(
    get_last_version_from_zenodo(
      doi = "zenodo.123456",
      pattern = "data.csv",
      path = "output/data.csv"
    ),
    "invalid Zenodo DOI format",
    class = "tima_validation_error"
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

  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.1234567",
      pattern = "data.csv",
      path = "output/data.csv",
      timeout_s = 0
    ),
    "timeout_s must be a single positive numeric value"
  )
})

test_that("get_last_version_from_zenodo handles network errors gracefully", {
  skip_if_offline()
  skip_on_cran()
  withr::local_options(list(
    tima.zenodo.max_attempts = 1L,
    tima.zenodo.req_retry_max_tries = 1L,
    tima.zenodo.backoff_cap_s = 0
  ))

  # Use invalid record that doesn't exist
  expect_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.9999999999",
      pattern = "test",
      path = temp_test_path("test.txt"),
      timeout_s = 1
    ),
    "Zenodo record not found|Failed to retrieve"
  )
})

test_that("get_last_version_from_zenodo handles file size comparison", {
  skip_if_offline()
  skip_on_cran()
  skip("Requires actual Zenodo download")

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
  skip("Requires actual Zenodo download")

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

  expect_equal(length(params), 4)
  expect_true("doi" %in% params)
  expect_true("pattern" %in% params)
  expect_true("path" %in% params)
  expect_true("timeout_s" %in% params)
  expect_equal(formals(get_last_version_from_zenodo)$timeout_s, 90)
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
    "expected format.*10\\.5281/zenodo",
    class = "tima_validation_error"
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
  withr::local_dir(new = tmp)
  expect_no_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.5794106",
      pattern = "frozen.csv.gz",
      path = file.path(tmp, "frozen.csv.gz")
    )
  )
  expect_no_error(
    get_last_version_from_zenodo(
      doi = "10.5281/zenodo.5794106",
      pattern = "frozen.csv.gz",
      path = file.path(tmp, "frozen.csv.gz")
    )
  )
})

tima_ns_fn <- function(name) {
  get(name, envir = asNamespace("tima"), inherits = FALSE)
}

make_zenodo_response <- function(json_text) {
  httr2::response(
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(json_text)
  )
}

test_that("validate_zenodo_inputs enforces timeout and accepts valid values", {
  validate_zenodo_inputs <- tima_ns_fn("validate_zenodo_inputs")

  expect_no_error(
    validate_zenodo_inputs(
      doi = "10.5281/zenodo.123456",
      pattern = "file.txt",
      path = "out/file.txt",
      timeout_s = 1
    )
  )

  expect_error(
    validate_zenodo_inputs(
      doi = "10.5281/zenodo.123456",
      pattern = "file.txt",
      path = "out/file.txt",
      timeout_s = 0
    ),
    "timeout_s must be a single positive numeric value",
    class = "tima_validation_error"
  )

  expect_error(
    validate_zenodo_inputs(
      doi = "10.5281/zenodo.123456",
      pattern = "file.txt",
      path = "out/file.txt",
      timeout_s = NA_real_
    ),
    "timeout_s must be a single positive numeric value",
    class = "tima_validation_error"
  )
})

test_that("extract_zenodo_record_id strips DOI prefix case-insensitively", {
  extract_zenodo_record_id <- tima_ns_fn("extract_zenodo_record_id")

  expect_identical(
    extract_zenodo_record_id("10.5281/zenodo.987654"),
    "987654"
  )

  expect_identical(
    extract_zenodo_record_id("10.5281/ZENODO.42"),
    "42"
  )
})

test_that("find_matching_file returns first match and errors on invalid pools", {
  find_matching_file <- tima_ns_fn("find_matching_file")

  expect_identical(
    find_matching_file(
      filenames = c("a.txt", "target.csv", "target.csv.gz"),
      pattern = "target",
      doi = "10.5281/zenodo.1"
    ),
    2L
  )

  expect_error(
    find_matching_file(
      filenames = character(0),
      pattern = "target",
      doi = "10.5281/zenodo.1"
    ),
    "no files found in Zenodo record",
    class = "tima_runtime_error"
  )

  expect_error(
    find_matching_file(
      filenames = c("a.txt", "b.txt"),
      pattern = "target",
      doi = "10.5281/zenodo.1"
    ),
    "no files matching pattern found in Zenodo record",
    class = "tima_validation_error"
  )
})

test_that("is_download_needed handles missing, mismatched, and matching file sizes", {
  is_download_needed <- tima_ns_fn("is_download_needed")

  missing_path <- tempfile(fileext = ".txt")
  expect_true(is_download_needed(path = missing_path, zenodo_size = 10))

  local_path <- tempfile(fileext = ".txt")
  writeLines("abcdef", con = local_path)

  expect_false(
    is_download_needed(path = local_path, zenodo_size = file.size(local_path))
  )
  expect_true(
    is_download_needed(
      path = local_path,
      zenodo_size = file.size(local_path) + 1
    )
  )
})

test_that("get_last_version_from_zenodo wraps JSON parse failures", {
  testthat::with_mocked_bindings(
    fetch_zenodo_record = function(record, doi, timeout_s) {
      list(not = "an httr2 response")
    },
    {
      expect_error(
        get_last_version_from_zenodo(
          doi = "10.5281/zenodo.123456",
          pattern = "target.txt",
          path = temp_test_path("zenodo-parse-fail.txt"),
          timeout_s = 1
        ),
        "failed to parse Zenodo API response",
        class = "tima_runtime_error"
      )
    },
    .package = "tima"
  )
})

test_that("get_last_version_from_zenodo errors when record has no files", {
  no_files_response <- make_zenodo_response('{"id":"123456"}')

  testthat::with_mocked_bindings(
    fetch_zenodo_record = function(record, doi, timeout_s) {
      no_files_response
    },
    {
      expect_error(
        get_last_version_from_zenodo(
          doi = "10.5281/zenodo.123456",
          pattern = "target.txt",
          path = temp_test_path("zenodo-no-files.txt"),
          timeout_s = 1
        ),
        "no files found in Zenodo record",
        class = "tima_runtime_error"
      )
    },
    .package = "tima"
  )
})

test_that("get_last_version_from_zenodo builds fallback URL and calls download helpers", {
  response_obj <- make_zenodo_response(
    paste0(
      '{"id":"7654321","metadata":{"title":"Demo"},"files":',
      '[{"key":"target.txt","size":7,"links":{}}]}'
    )
  )

  tmp_path <- temp_test_path("zenodo-fallback-download.txt")
  writeLines("old", con = tmp_path)

  captured_url <- NULL
  captured_export <- NULL
  created_dir <- FALSE

  testthat::with_mocked_bindings(
    fetch_zenodo_record = function(record, doi, timeout_s) {
      response_obj
    },
    is_download_needed = function(path, zenodo_size) {
      TRUE
    },
    create_dir = function(export) {
      created_dir <<- TRUE
      invisible(export)
    },
    get_file = function(url, export) {
      captured_url <<- url
      captured_export <<- export
      invisible(export)
    },
    {
      result <- get_last_version_from_zenodo(
        doi = "10.5281/zenodo.123456",
        pattern = "target.txt",
        path = tmp_path,
        timeout_s = 1
      )

      expect_identical(result, tmp_path)
      expect_true(created_dir)
      expect_identical(captured_export, tmp_path)
      expect_identical(
        captured_url,
        "https://zenodo.org/api/records/7654321/files/target.txt/content"
      )
    },
    .package = "tima"
  )
})

test_that("get_last_version_from_zenodo skips download when local file is current", {
  response_obj <- make_zenodo_response(
    paste0(
      '{"recid":"888","files":',
      '[{"key":"target.txt","size":11,"links":{"download":"https://example/target.txt"}}]}'
    )
  )

  testthat::with_mocked_bindings(
    fetch_zenodo_record = function(record, doi, timeout_s) {
      response_obj
    },
    is_download_needed = function(path, zenodo_size) {
      FALSE
    },
    create_dir = function(export) {
      stop("create_dir should not be called when download is skipped")
    },
    get_file = function(url, export) {
      stop("get_file should not be called when download is skipped")
    },
    {
      expect_identical(
        get_last_version_from_zenodo(
          doi = "10.5281/zenodo.123456",
          pattern = "target.txt",
          path = temp_test_path("zenodo-skip.txt"),
          timeout_s = 1
        ),
        temp_test_path("zenodo-skip.txt")
      )
    },
    .package = "tima"
  )
})
