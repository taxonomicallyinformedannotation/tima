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
    "Provide a non-empty string."
  )

  # Non-character URL
  expect_error(
    get_file(url = 123, export = "test.txt"),
    "Ensure the parameter is a length-1 character value."
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
    "Provide a non-NULL character string."
  )

  # Empty export
  expect_error(
    get_file(url = "https://example.com/file.txt", export = ""),
    "Provide a non-empty string."
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
  withr::with_dir(tmp, {
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
})

## Directory Creation ----

test_that("get_file creates output directory if needed", {
  tmp <- temp_test_dir("get_file_dir_create")
  withr::with_dir(tmp, {
    paths <- local_test_project(copy = TRUE)
    test_url <- paste0(
      "https://raw.githubusercontent.com/",
      "taxonomicallyinformedannotation/tima-example-files/",
      "main/README.md"
    )
    nested_path <- file.path("data", "deep", "nested", "dir", "file.txt")

    result <- get_file(url = test_url, export = nested_path)

    expect_true(file.exists(nested_path))
    expect_true(dir.exists(dirname(nested_path)))
  })
})

## Download Success ----

test_that("get_file downloads file successfully", {
  tmp <- temp_test_dir("get_file_download_success")
  withr::with_dir(tmp, {
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
})

## Download Failure Handling ----

test_that("get_file fails gracefully with invalid URL", {
  tmp <- temp_test_dir("get_file_invalid_url")
  withr::with_dir(tmp, {
    paths <- local_test_project(copy = TRUE)
    test_file <- file.path("data", "source", "test.txt")
    if (file.exists(test_file)) unlink(test_file, force = TRUE)

    expect_error(
      get_file(
        url = "https://thissitedoesnotexist123456789.com/file.txt",
        export = test_file,
        limit = 5
      ),
      "Failed to download"
    )
    expect_false(file.exists(test_file))
  })
})

test_that("get_file handles 404 errors", {
  tmp <- temp_test_dir("get_file_404")
  withr::with_dir(tmp, {
    paths <- local_test_project(copy = TRUE)
    test_file <- file.path("data", "source", "test.txt")
    if (file.exists(test_file)) unlink(test_file, force = TRUE)

    expect_error(
      get_file(
        url = "https://github.com/nonexistent/repo/raw/main/missing.txt",
        export = test_file,
        limit = 10
      ),
      "Failed to download"
    )
  })
})

## Integration with get_example_files ----

test_that("get_file works with real package example files", {
  tmp <- temp_test_dir("get_file_examples")
  withr::with_dir(tmp, {
    paths <- local_test_project(copy = TRUE)
    expect_no_error(
      get_file(
        url = paths$urls$examples$features,
        export = file.path("data", "source", "features.csv")
      )
    )
    expect_true(file.exists(file.path("data", "source", "features.csv")))
  })
})

## Pollution Check ----

test_that("get_file tests do not pollute tests/testthat directory", {
  # Ensure no data directory was created at testthat root
  expect_false(dir.exists(file.path("..", "..", "tests", "testthat", "data")))
})
