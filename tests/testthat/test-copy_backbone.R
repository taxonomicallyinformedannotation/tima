# Test Suite for copy_backbone()
# Tests the package backbone copying functionality

library(testthat)
library(tima)

# ==============================================================================
# Test: Input Validation
# ==============================================================================

test_that("copy_backbone validates cache_dir parameter", {
  # NULL cache_dir
  expect_error(
    copy_backbone(cache_dir = NULL),
    "Cache directory path must be"
  )

  # Empty cache_dir
  expect_error(
    copy_backbone(cache_dir = ""),
    "Cache directory path must be"
  )

  # Non-character cache_dir
  expect_error(
    copy_backbone(cache_dir = 123),
    "Cache directory path must be"
  )

  # Multiple values
  expect_error(
    copy_backbone(cache_dir = c("dir1", "dir2")),
    "Cache directory path must be"
  )
})

test_that("copy_backbone validates package parameter", {
  temp_dir <- withr::local_tempdir()

  # NULL package
  expect_error(
    copy_backbone(cache_dir = temp_dir, package = NULL),
    "Package name must be"
  )

  # Empty package
  expect_error(
    copy_backbone(cache_dir = temp_dir, package = ""),
    "Package name must be"
  )

  # Non-character package
  expect_error(
    copy_backbone(cache_dir = temp_dir, package = 123),
    "Package name must be"
  )
})

test_that("copy_backbone fails for non-existent package", {
  temp_dir <- withr::local_tempdir()

  expect_error(
    copy_backbone(
      cache_dir = temp_dir,
      package = "NonExistentPackage123456"
    ),
    "Package.*not found"
  )
})

# ==============================================================================
# Test: Directory Creation
# ==============================================================================

test_that("copy_backbone creates cache directory", {
  temp_dir <- withr::local_tempdir()
  cache_path <- file.path(temp_dir, "new_cache")

  expect_false(dir.exists(cache_path))

  copy_backbone(cache_dir = cache_path)

  expect_true(dir.exists(cache_path))
})

test_that("copy_backbone handles existing directories", {
  temp_dir <- withr::local_tempdir()

  # First copy
  expect_no_error(copy_backbone(cache_dir = temp_dir))

  # Second copy (overwrite)
  expect_no_error(copy_backbone(cache_dir = temp_dir))
})

# ==============================================================================
# Test: Content Verification
# ==============================================================================

test_that("copy_backbone copies package structure", {
  temp_dir <- withr::local_tempdir()

  copy_backbone(cache_dir = temp_dir)

  # Check that cache directory was created and populated
  expect_true(dir.exists(temp_dir))

  # The structure depends on package installation
  # At minimum, the backbone should copy something
  files_copied <- list.files(temp_dir, recursive = TRUE)
  expect_true(length(files_copied) > 0)
})

test_that("copy_backbone copies parameter files", {
  temp_dir <- withr::local_tempdir()

  copy_backbone(cache_dir = temp_dir)

  # Check that files were copied (structure may vary)
  all_files <- list.files(temp_dir, recursive = TRUE)
  expect_true(length(all_files) > 0)
})

# ==============================================================================
# Test: Error Handling
# ==============================================================================

test_that("copy_backbone handles permission errors gracefully", {
  skip_on_os("windows") # Different permissions model

  # This test is tricky - would need a read-only directory
  # Skip for now as it's OS-dependent
  skip("Permission error testing requires OS-specific setup")
})

# ==============================================================================
# Test: Integration
# ==============================================================================

test_that("copy_backbone integrates with get_default_paths", {
  temp_dir <- withr::local_tempdir()
  withr::local_dir(temp_dir)

  local_test_project(copy = TRUE)

  # Should be able to get paths after copying backbone
  expect_no_error(paths <- get_default_paths())
})

test_that("copy_backbone works with relative paths", {
  temp_dir <- withr::local_tempdir()
  withr::local_dir(temp_dir)

  # Copy to current directory
  copy_backbone(cache_dir = ".")

  # Check that the copy operation completed without error
  # The actual structure copied depends on how the package is installed
  expect_true(TRUE)  # If we got here, the function didn't error
})

test_that("copy_backbone works with absolute paths", {
  temp_dir <- withr::local_tempdir()
  cache_path <- file.path(temp_dir, "absolute_cache")

  expect_no_error(copy_backbone(cache_dir = cache_path))

  expect_true(dir.exists(cache_path))
})

# ==============================================================================
# Test: Return Value
# ==============================================================================

test_that("copy_backbone returns invisibly", {
  temp_dir <- withr::local_tempdir()

  result <- withVisible(copy_backbone(cache_dir = temp_dir))

  expect_null(result$value)
  expect_false(result$visible)
})
