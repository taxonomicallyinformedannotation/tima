# Test Suite for get_file()
# Tests the file download function with retry logic and validation

library(testthat)
library(tima)

# ==============================================================================
# Test: Input Validation
# ==============================================================================

test_that("get_file validates URL parameter", {
  # Missing URL
  expect_error(
    get_file(export = "test.txt"),
    "URL must be"
  )

  # NULL URL
  expect_error(
    get_file(url = NULL, export = "test.txt"),
    "URL must be"
  )

  # Empty URL
  expect_error(
    get_file(url = "", export = "test.txt"),
    "URL must be"
  )

  # Non-character URL
  expect_error(
    get_file(url = 123, export = "test.txt"),
    "URL must be"
  )
})

test_that("get_file validates export parameter", {
  # Missing export
  expect_error(
    get_file(url = "https://example.com/file.txt"),
    "Export path must be"
  )

  # NULL export
  expect_error(
    get_file(url = "https://example.com/file.txt", export = NULL),
    "Export path must be"
  )

  # Empty export
  expect_error(
    get_file(url = "https://example.com/file.txt", export = ""),
    "Export path must be"
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
    "must be a positive number"
  )

  # Zero timeout
  expect_error(
    get_file(
      url = "https://example.com/file.txt",
      export = "test.txt",
      limit = 0
    ),
    "must be a positive number"
  )
})

# ==============================================================================
# Test: File Existence Check (Early Exit)
# ==============================================================================

test_that("get_file skips download if file already exists", {
  paths <- local_test_project(copy = TRUE)

  # Create a dummy file
  test_file <- file.path("data", "source", "existing_file.txt")
  dir.create(dirname(test_file), recursive = TRUE, showWarnings = FALSE)
  writeLines("test content", test_file)

  # Should return immediately without downloading
  result <- get_file(
    url = "https://example.com/file.txt",
    export = test_file
  )

  expect_equal(result, test_file)
  expect_true(file.exists(test_file))

  # Content should be unchanged (no actual download)
  content <- readLines(test_file)
  expect_equal(content, "test content")
})

# ==============================================================================
# Test: Directory Creation
# ==============================================================================

test_that("get_file creates output directory if needed", {
  skip_on_cran()
  skip_if_offline()

  paths <- local_test_project(copy = TRUE)

  # Use a valid small file from GitHub
  test_url <- paste0(
    "https://raw.githubusercontent.com/",
    "taxonomicallyinformedannotation/tima-example-files/",
    "main/README.md"
  )

  nested_path <- file.path("data", "deep", "nested", "dir", "file.txt")

  # Directory should be created automatically
  result <- get_file(url = test_url, export = nested_path)

  expect_true(file.exists(nested_path))
  expect_true(dir.exists(dirname(nested_path)))
})

# ==============================================================================
# Test: Download Success
# ==============================================================================

test_that("get_file downloads file successfully", {
  skip_on_cran()
  skip_if_offline()

  paths <- local_test_project(copy = TRUE)

  # Small test file
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

# ==============================================================================
# Test: Download Failure Handling
# ==============================================================================

test_that("get_file fails gracefully with invalid URL", {
  skip_on_cran()

  paths <- local_test_project(copy = TRUE)

  expect_error(
    get_file(
      url = "https://thissitedoesnotexist123456789.com/file.txt",
      export = "data/source/test.txt",
      limit = 5
    ),
    "Failed to download"
  )

  # Partial file should be cleaned up
  expect_false(file.exists("data/source/test.txt"))
})

test_that("get_file handles 404 errors", {
  skip_on_cran()
  skip_if_offline()

  paths <- local_test_project(copy = TRUE)

  expect_error(
    get_file(
      url = "https://github.com/nonexistent/repo/raw/main/missing.txt",
      export = "data/source/test.txt",
      limit = 10
    ),
    "Failed to download"
  )
})

# ==============================================================================
# Test: Retry Logic
# ==============================================================================

test_that("get_file retries on failure", {
  skip("Manual test - requires network manipulation")

  # This test would require mocking network failures
  # or using a test server that can simulate retries
})

# ==============================================================================
# Test: Integration with get_example_files
# ==============================================================================

test_that("get_file works with real package example files", {
  skip_on_cran()
  skip_if_offline()

  paths <- local_test_project(copy = TRUE)

  # Use actual example file URL
  expect_no_error(
    get_file(
      url = paths$urls$examples$features,
      export = paths$data$source$features
    )
  )

  expect_true(file.exists(paths$data$source$features))
})
