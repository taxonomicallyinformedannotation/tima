# Test Suite: get_file ----

library(testthat)

## Input Validation ----

test_that("get_file validates URL parameter", {
  # Missing URL
  expect_error(
    get_file(export = "test.txt"),
    "argument \"url\" is missing, with no default"
  )

  # NULL URL
  expect_error(
    get_file(url = NULL, export = "test.txt"),
    "url cannot be NULL"
  )

  # Empty URL
  expect_error(
    get_file(url = "", export = "test.txt"),
    "Fix: Provide a non-empty string",
    fixed = TRUE
  )

  # Non-character URL
  expect_error(
    get_file(url = 123, export = "test.txt"),
    "Fix: Ensure the parameter is a length-1 character value",
    fixed = TRUE
  )
})

test_that("get_file validates export parameter", {
  # Missing export
  expect_error(
    get_file(url = "https://example.com/file.txt"),
    "argument \"export\" is missing, with no default"
  )

  # NULL export
  expect_error(
    get_file(url = "https://example.com/file.txt", export = NULL),
    "Fix: Provide a non-NULL character string",
    fixed = TRUE
  )

  # Empty export
  expect_error(
    get_file(url = "https://example.com/file.txt", export = ""),
    "Fix: Provide a non-empty string",
    fixed = TRUE
  )
})

test_that("get_file validates timeout limit", {
  # Negative timeout
  expect_error(
    get_file(
      url = "https://example.com/file.txt",
      export = "test.txt",
      limit = -100
    ),
    "limit must be > 0, got: -100"
  )

  # Zero timeout
  expect_error(
    get_file(
      url = "https://example.com/file.txt",
      export = "test.txt",
      limit = 0
    ),
    "limit must be > 0, got: 0"
  )
})

## File Existence Check ----

test_that("get_file skips download if file already exists", {
  tmp <- temp_test_dir("get_file_exists")
  withr::local_dir(new = tmp)
  paths <- local_test_project(copy = TRUE)
  test_file <- file.path("data", "source", "existing_file.txt")
  dir.create(dirname(test_file), recursive = TRUE, showWarnings = FALSE)
  writeLines("test content", test_file)

  result <- get_file(
    url = "https://example.com/file.txt",
    export = test_file
  )

  expect_equal(result, test_file)
  expect_true(file.exists(test_file))
  expect_equal(readLines(test_file), "test content")
})

## Directory Creation ----

test_that("get_file creates output directory if needed", {
  skip_on_cran()
  skip_if_offline()
  tmp <- temp_test_dir("get_file_create_dir")
  withr::local_dir(new = tmp)
  paths <- local_test_project(copy = TRUE)
  test_url <- paste0(
    "https://raw.githubusercontent.com/",
    "taxonomicallyinformedannotation/tima-example-files/",
    "main/README.md"
  )
  nested_path <- file.path("data", "deep", "nested", "dir", "file.txt")

  expect_no_error({
    result <- get_file(
      url = test_url,
      export = nested_path
    )
  })

  expect_true(file.exists(nested_path))
})

## Helper Function Tests ----

test_that("validate_http_response detects 404 errors", {
  # Create mock response with 404 status
  mock_resp <- structure(
    list(status_code = 404L),
    class = "httr2_response"
  )

  expect_error(
    validate_http_response(
      mock_resp,
      "http://example.com/missing",
      "output.txt"
    ),
    "Resource not found"
  )
})

test_that("validate_http_response detects 403 errors", {
  mock_resp <- structure(
    list(status_code = 403L),
    class = "httr2_response"
  )

  expect_error(
    validate_http_response(
      mock_resp,
      "http://example.com/forbidden",
      "output.txt"
    ),
    "Access forbidden"
  )
})

test_that("validate_http_response detects 500 errors", {
  mock_resp <- structure(
    list(status_code = 500L),
    class = "httr2_response"
  )

  expect_error(
    validate_http_response(mock_resp, "http://example.com/error", "output.txt"),
    "Server error"
  )
})

test_that("validate_http_response detects redirect codes", {
  mock_resp_301 <- structure(
    list(status_code = 301L),
    class = "httr2_response"
  )

  expect_error(
    validate_http_response(
      mock_resp_301,
      "http://example.com/moved",
      "output.txt"
    ),
    "Resource moved"
  )
})

test_that("validate_http_response passes on success status", {
  mock_resp <- structure(
    list(status_code = 200L),
    class = "httr2_response"
  )

  expect_no_error(
    validate_http_response(mock_resp, "http://example.com/ok", "output.txt")
  )
})

test_that("validate_downloaded_file detects missing files", {
  expect_error(
    validate_downloaded_file(
      "/nonexistent/file.txt",
      "http://example.com/file"
    ),
    "Download completed but file not created"
  )
})

test_that("validate_downloaded_file passes for valid files", {
  tmp_file <- temp_test_path("valid.txt")
  writeLines("some content here", tmp_file)

  expect_no_error(
    validate_downloaded_file(tmp_file, "http://example.com/file")
  )
})

test_that("is_valid_binary_format detects ZIP files", {
  # ZIP magic bytes: PK (0x50 0x4B)
  zip_header <- as.raw(c(0x50, 0x4B, 0x03, 0x04))
  expect_true(is_valid_binary_format(zip_header))
})

test_that("is_valid_binary_format detects GZIP files", {
  # GZIP magic bytes: 0x1F 0x8B
  gzip_header <- as.raw(c(0x1F, 0x8B, 0x08, 0x00))
  expect_true(is_valid_binary_format(gzip_header))
})

test_that("is_valid_binary_format rejects text", {
  text_header <- charToRaw("This is text content")
  expect_false(is_valid_binary_format(text_header))
})

test_that("is_valid_binary_format handles short headers", {
  short_header <- as.raw(c(0x50))
  expect_false(is_valid_binary_format(short_header))

  empty_header <- raw(0)
  expect_false(is_valid_binary_format(empty_header))
})

test_that("is_html_content detects HTML documents", {
  html_header <- charToRaw("<!DOCTYPE html><html>")
  expect_true(is_html_content(html_header))

  html_lower <- charToRaw("<html><head>")
  expect_true(is_html_content(html_lower))

  html_upper <- charToRaw("<HTML><HEAD>")
  expect_true(is_html_content(html_upper))
})

test_that("is_html_content rejects non-HTML content", {
  text_header <- charToRaw("This is plain text")
  expect_false(is_html_content(text_header))

  csv_header <- charToRaw("id,name,value\n1,test,123")
  expect_false(is_html_content(csv_header))

  empty_header <- raw(0)
  expect_false(is_html_content(empty_header))
})

test_that("is_html_content handles whitespace before HTML", {
  html_ws <- charToRaw("\n\n  <!doctype html>")
  expect_true(is_html_content(html_ws))
})

test_that("validate_file_content detects HTML error pages", {
  tmp_file <- temp_test_path("error.html")
  writeLines("<html><head><title>404 Error</title></head></html>", tmp_file)

  expect_error(
    validate_file_content(tmp_file, "http://example.com/file"),
    "Server returned HTML error page"
  )
})

test_that("validate_file_content passes for ZIP files", {
  # Create a minimal ZIP file
  tmp_dir <- temp_test_path("ziptest")
  dir.create(tmp_dir, recursive = TRUE)
  tmp_txt <- file.path(tmp_dir, "test.txt")
  writeLines("content", tmp_txt)

  zip_file <- temp_test_path("test.zip")
  withr::with_dir(tmp_dir, {
    utils::zip(zipfile = zip_file, files = "test.txt")
  })

  expect_no_error(
    validate_file_content(zip_file, "http://example.com/file.zip")
  )
})

test_that("cleanup_failed_download removes files", {
  tmp_file <- temp_test_path("cleanup_test.txt")
  writeLines("test", tmp_file)
  expect_true(file.exists(tmp_file))

  cleanup_failed_download(tmp_file)
  expect_false(file.exists(tmp_file))
})

test_that("cleanup_failed_download handles nonexistent files", {
  expect_no_error(
    cleanup_failed_download("/nonexistent/file.txt")
  )
})

## Download Success ----

test_that("get_file downloads file successfully", {
  tmp <- temp_test_dir("get_file_download_success")
  withr::local_dir(new = tmp)
  paths <- local_test_project(copy = TRUE)
  test_url <- paste0(
    "https://raw.githubusercontent.com/",
    "taxonomicallyinformedannotation/tima-example-files/",
    "main/README.md"
  )
  output_file <- file.path("data", "source", "test_download.txt")

  result <- get_file(url = test_url, export = output_file)

  expect_equal(result, output_file)
  expect_true(file.exists(output_file))
  expect_gt(file.size(output_file), 0)
})

## Download Failure Handling ----

test_that("get_file fails gracefully with invalid URL", {
  # skip("Network test - too slow with retry logic")

  tmp <- temp_test_dir("get_file_invalid")
  withr::local_dir(new = tmp)
  paths <- local_test_project(copy = TRUE)
  test_file <- file.path("data", "source", "test.txt")
  if (file.exists(test_file)) {
    unlink(test_file, force = TRUE)
  }

  expect_error(
    get_file(
      url = "https://thissitedoesnotexist123456789.com/file.txt",
      export = test_file,
      limit = 5
    ),
    "failed after retries"
  )
  expect_false(file.exists(test_file))
})

test_that("get_file handles 404 errors", {
  # skip("Network test - too slow with retry logic")

  tmp <- temp_test_dir("get_file_404")
  withr::local_dir(new = tmp)
  paths <- local_test_project(copy = TRUE)
  test_file <- file.path("data", "source", "test.txt")
  if (file.exists(test_file)) {
    unlink(test_file, force = TRUE)
  }

  expect_error(
    get_file(
      url = "https://github.com/nonexistent/repo/raw/main/missing.txt",
      export = test_file,
      limit = 10
    ),
    "failed after retries"
  )
})

## Integration with get_example_files ----

test_that("get_file works with real package example files", {
  tmp <- temp_test_dir("get_file_examples")
  withr::local_dir(new = tmp)
  paths <- local_test_project(copy = TRUE)
  expect_no_error(
    get_file(
      url = paths$urls$examples$features,
      export = file.path("data", "source", "features.csv")
    )
  )
  expect_true(file.exists(file.path("data", "source", "features.csv")))
})
