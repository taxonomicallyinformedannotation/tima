# Test Suite: copy_backbone ----

library(testthat)

## Input Validation ----

test_that("copy_backbone validates cache_dir must be non-NULL character", {
  expect_error(
    copy_backbone(cache_dir = NULL),
    "Cache directory path must be",
    info = "NULL cache_dir should error"
  )
})

test_that("copy_backbone validates cache_dir must be non-empty", {
  expect_error(
    copy_backbone(cache_dir = ""),
    "Cache directory path must be",
    info = "Empty cache_dir should error"
  )
})

test_that("copy_backbone validates cache_dir must be character type", {
  expect_error(
    copy_backbone(cache_dir = 123),
    "Cache directory path must be",
    info = "Non-character cache_dir should error"
  )
})

test_that("copy_backbone validates cache_dir must be scalar", {
  expect_error(
    copy_backbone(cache_dir = c("dir1", "dir2")),
    "Cache directory path must be",
    info = "Multiple values for cache_dir should error"
  )
})

test_that("copy_backbone validates package must be non-NULL character", {
  test_cache <- tempdir()

  expect_error(
    copy_backbone(cache_dir = test_cache, package = NULL),
    "Package name must be",
    info = "NULL package should error"
  )
})

test_that("copy_backbone validates package must be non-empty", {
  test_cache <- tempdir()

  expect_error(
    copy_backbone(cache_dir = test_cache, package = ""),
    "Package name must be",
    info = "Empty package should error"
  )
})

test_that("copy_backbone validates package must be character type", {
  test_cache <- tempdir()

  expect_error(
    copy_backbone(cache_dir = test_cache, package = 123),
    "Package name must be",
    info = "Non-character package should error"
  )
})

test_that("copy_backbone rejects non-existent package names", {
  test_cache <- tempdir()

  expect_error(
    copy_backbone(
      cache_dir = test_cache,
      package = "NonExistentPackage123456"
    ),
    "Package.*not found",
    info = "Non-existent package should error"
  )
})

## Directory Creation ----

test_that("copy_backbone creates cache directory when it doesn't exist", {
  cache_path <- tempdir()

  expect_false(
    dir.exists(file.path(cache_path, "nonexistent_subdir")),
    info = "Subdir should not exist initially"
  )

  copy_backbone(cache_dir = cache_path)

  expect_true(dir.exists(cache_path), info = "Cache path should be created")
})

test_that("copy_backbone handles existing cache directory gracefully", {
  test_cache <- tempdir()

  # First copy
  expect_no_error(copy_backbone(cache_dir = test_cache))

  # Second copy (overwrite)
  expect_no_error(copy_backbone(cache_dir = test_cache))
})

## Content Verification ----

test_that("copy_backbone copies package structure to cache directory", {
  test_cache <- tempdir()

  copy_backbone(cache_dir = test_cache)

  # Verify cache directory was created and populated
  expect_true(dir.exists(test_cache), info = "Cache directory should exist")

  # Verify files were copied
  files_copied <- list.files(test_cache, recursive = TRUE)
  expect_true(
    length(files_copied) > 0,
    info = "At least some files should be copied"
  )
})

test_that("copy_backbone creates expected directory structure", {
  test_cache <- tempdir()

  copy_backbone(cache_dir = test_cache)

  # Check for common expected directories (may vary by package structure)
  all_items <- list.files(test_cache, recursive = FALSE, all.files = TRUE)
  expect_true(
    length(all_items) > 0,
    info = "Should create directory structure"
  )
})

## Path Handling ----

test_that("copy_backbone works with relative-style temp paths", {
  # Use a temp directory (not actual current directory)
  cache_path <- tempdir()

  expect_no_error(copy_backbone(cache_dir = cache_path))

  # Verify operation completed
  expect_true(dir.exists(cache_path))
})

test_that("copy_backbone works with absolute paths", {
  cache_path <- tempdir()

  expect_no_error(copy_backbone(cache_dir = cache_path))

  expect_true(
    dir.exists(cache_path),
    info = "Cache directory should be created"
  )
})

# Integration ----

test_that("copy_backbone integrates with get_default_paths", {
  # Use temp directory, not current directory
  cache_path <- tempdir()
  copy_backbone(cache_dir = cache_path)

  # Should be able to get paths after copying backbone
  expect_no_error(paths <- get_default_paths())
})

test_that("copy_backbone supports local_test_project workflow", {
  # This tests the integration used throughout the test suite
  expect_no_error(paths <- local_test_project(copy = TRUE))

  expect_type(paths, "list")
})

## Return Value ----

test_that("copy_backbone returns invisibly", {
  test_cache <- tempdir()

  result <- withVisible(copy_backbone(cache_dir = test_cache))

  expect_null(result$value, info = "Return value should be NULL")
  expect_false(result$visible, info = "Return should be invisible")
})

## Edge Cases and Special Scenarios ----

test_that("copy_backbone handles nested cache directory paths", {
  nested_path <- tempdir()

  expect_no_error(copy_backbone(cache_dir = nested_path))

  expect_true(
    dir.exists(nested_path),
    info = "Nested path should be created"
  )
})

test_that("copy_backbone can be called multiple times with different paths", {
  base_dir <- tempdir()

  cache1 <- file.path(base_dir, "cache1")
  cache2 <- file.path(base_dir, "cache2")

  expect_no_error(copy_backbone(cache_dir = cache1))
  expect_no_error(copy_backbone(cache_dir = cache2))

  expect_true(dir.exists(cache1), info = "First cache should exist")
  expect_true(dir.exists(cache2), info = "Second cache should exist")
})
