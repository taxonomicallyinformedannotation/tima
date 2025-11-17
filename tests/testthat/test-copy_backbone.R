# ==============================================================================
# Test Suite: copy_backbone
# ==============================================================================
#
# @description
# Unit tests for the copy_backbone function, which copies the package directory
# structure to a cache location for project initialization.
#
# @coverage
# - Input validation (cache_dir, package parameters)
# - Directory creation and handling
# - Content verification
# - Integration with get_default_paths
# - Error handling
# - Return value behavior

# ==============================================================================
# Input Validation Tests
# ==============================================================================

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
  temp_dir <- withr::local_tempdir()

  expect_error(
    copy_backbone(cache_dir = temp_dir, package = NULL),
    "Package name must be",
    info = "NULL package should error"
  )
})

test_that("copy_backbone validates package must be non-empty", {
  temp_dir <- withr::local_tempdir()

  expect_error(
    copy_backbone(cache_dir = temp_dir, package = ""),
    "Package name must be",
    info = "Empty package should error"
  )
})

test_that("copy_backbone validates package must be character type", {
  temp_dir <- withr::local_tempdir()

  expect_error(
    copy_backbone(cache_dir = temp_dir, package = 123),
    "Package name must be",
    info = "Non-character package should error"
  )
})

test_that("copy_backbone rejects non-existent package names", {
  temp_dir <- withr::local_tempdir()

  expect_error(
    copy_backbone(
      cache_dir = temp_dir,
      package = "NonExistentPackage123456"
    ),
    "Package.*not found",
    info = "Non-existent package should error"
  )
})

# ==============================================================================
# Directory Creation Tests
# ==============================================================================

test_that("copy_backbone creates cache directory when it doesn't exist", {
  temp_dir <- withr::local_tempdir()
  cache_path <- file.path(temp_dir, "new_cache")

  expect_false(
    dir.exists(cache_path),
    info = "Cache path should not exist initially"
  )

  copy_backbone(cache_dir = cache_path)

  expect_true(dir.exists(cache_path), info = "Cache path should be created")
})

test_that("copy_backbone handles existing cache directory gracefully", {
  temp_dir <- withr::local_tempdir()

  # First copy
  expect_no_error(copy_backbone(cache_dir = temp_dir))

  # Second copy (overwrite)
  expect_no_error(copy_backbone(cache_dir = temp_dir))
})

# ==============================================================================
# Content Verification Tests
# ==============================================================================

test_that("copy_backbone copies package structure to cache directory", {
  temp_dir <- withr::local_tempdir()

  copy_backbone(cache_dir = temp_dir)

  # Verify cache directory was created and populated
  expect_true(dir.exists(temp_dir), info = "Cache directory should exist")

  # Verify files were copied
  files_copied <- list.files(temp_dir, recursive = TRUE)
  expect_true(
    length(files_copied) > 0,
    info = "At least some files should be copied"
  )
})

test_that("copy_backbone creates expected directory structure", {
  temp_dir <- withr::local_tempdir()

  copy_backbone(cache_dir = temp_dir)

  # Check for common expected directories (may vary by package structure)
  all_items <- list.files(temp_dir, recursive = FALSE, all.files = TRUE)
  expect_true(
    length(all_items) > 0,
    info = "Should create directory structure"
  )
})

# ==============================================================================
# Path Handling Tests
# ==============================================================================

test_that("copy_backbone works with relative paths", {
  temp_dir <- withr::local_tempdir()
  withr::local_dir(temp_dir)

  # Copy to current directory
  expect_no_error(copy_backbone(cache_dir = "."))

  # Verify operation completed
  expect_true(TRUE)
})

test_that("copy_backbone works with absolute paths", {
  temp_dir <- withr::local_tempdir()
  cache_path <- file.path(temp_dir, "absolute_cache")

  expect_no_error(copy_backbone(cache_dir = cache_path))

  expect_true(
    dir.exists(cache_path),
    info = "Cache directory should be created"
  )
})

# ==============================================================================
# Integration Tests
# ==============================================================================

test_that("copy_backbone integrates with get_default_paths after copying", {
  temp_dir <- withr::local_tempdir()
  withr::local_dir(temp_dir)

  copy_backbone(cache_dir = ".")

  # Should be able to get paths after copying backbone
  expect_no_error(paths <- get_default_paths())
})

test_that("copy_backbone supports local_test_project workflow", {
  # This tests the integration used throughout the test suite
  expect_no_error(paths <- local_test_project(copy = TRUE))

  expect_type(paths, "list")
})

# ==============================================================================
# Return Value Tests
# ==============================================================================

test_that("copy_backbone returns invisibly", {
  temp_dir <- withr::local_tempdir()

  result <- withVisible(copy_backbone(cache_dir = temp_dir))

  expect_null(result$value, info = "Return value should be NULL")
  expect_false(result$visible, info = "Return should be invisible")
})

# ==============================================================================
# Edge Cases and Special Scenarios
# ==============================================================================

test_that("copy_backbone handles nested cache directory paths", {
  temp_dir <- withr::local_tempdir()
  nested_path <- file.path(temp_dir, "level1", "level2", "cache")

  expect_no_error(copy_backbone(cache_dir = nested_path))

  expect_true(
    dir.exists(nested_path),
    info = "Nested path should be created"
  )
})

test_that("copy_backbone can be called multiple times with different paths", {
  temp_dir <- withr::local_tempdir()

  cache1 <- file.path(temp_dir, "cache1")
  cache2 <- file.path(temp_dir, "cache2")

  expect_no_error(copy_backbone(cache_dir = cache1))
  expect_no_error(copy_backbone(cache_dir = cache2))

  expect_true(dir.exists(cache1), info = "First cache should exist")
  expect_true(dir.exists(cache2), info = "Second cache should exist")
})
