# Test Suite: go_to_cache ----

library(testthat)

# Input Validation Tests ----

test_that("go_to_cache validates dir parameter", {
  expect_error(
    go_to_cache(dir = NULL),
    "Cache directory name must be a non-empty character string"
  )

  expect_error(
    go_to_cache(dir = 123),
    "Cache directory name must be a non-empty character string"
  )

  expect_error(
    go_to_cache(dir = c(".cache1", ".cache2")),
    "Cache directory name must be a non-empty character string"
  )

  expect_error(
    go_to_cache(dir = ""),
    "Cache directory name must be a non-empty character string"
  )

  expect_error(
    go_to_cache(dir = character(0)),
    "Cache directory name must be a non-empty character string"
  )
})

# test_that("go_to_cache accepts valid directory names", {
#   old_wd <- getwd()
#   on.exit(setwd(old_wd), add = TRUE)
#
#   # Default directory name
#   expect_invisible(go_to_cache(dir = ".test_tima_cache_1"))
#
#   # Clean up
#   setwd(old_wd)
#   unlink(fs::path_home(".test_tima_cache_1"), recursive = TRUE)
# })

# Directory Creation Tests ----

# test_that("go_to_cache creates cache directory when it doesn't exist", {
#   old_wd <- getwd()
#   on.exit(setwd(old_wd), add = TRUE)
#
#   cache_dir <- ".test_tima_new_cache"
#   cache_path <- fs::path_home(cache_dir)
#
#   # Ensure it doesn't exist
#   if (dir.exists(cache_path)) {
#     unlink(cache_path, recursive = TRUE)
#   }
#
#   expect_false(dir.exists(cache_path))
#
#   # Should create it
#   result <- go_to_cache(dir = cache_dir)
#
#   expect_true(dir.exists(cache_path))
#   expect_equal(result, cache_path)
#
#   # Clean up
#   setwd(old_wd)
#   unlink(cache_path, recursive = TRUE)
# })
#
# test_that("go_to_cache works when cache directory already exists", {
#   old_wd <- getwd()
#   on.exit(setwd(old_wd), add = TRUE)
#
#   cache_dir <- ".test_tima_existing_cache"
#   cache_path <- fs::path_home(cache_dir)
#
#   # Create it first
#   fs::dir_create(cache_path)
#   expect_true(dir.exists(cache_path))
#
#   # Should work without errors
#   expect_invisible(go_to_cache(dir = cache_dir))
#   expect_equal(getwd(), cache_path |> as.character())
#
#   # Clean up
#   setwd(old_wd)
#   unlink(cache_path, recursive = TRUE)
# })
#
# # Working Directory Tests ----
#
# test_that("go_to_cache changes working directory", {
#   old_wd <- getwd()
#   on.exit(setwd(old_wd), add = TRUE)
#
#   cache_dir <- ".test_tima_wd_change"
#   cache_path <- fs::path_home(cache_dir)
#
#   go_to_cache(dir = cache_dir)
#
#   expect_equal(getwd(), cache_path)
#   expect_true(normalizePath(getwd()) == normalizePath(cache_path |> as.character()))
#
#   # Clean up
#   setwd(old_wd)
#   unlink(cache_path, recursive = TRUE)
# })
#
# test_that("go_to_cache returns cache path invisibly", {
#   old_wd <- getwd()
#   on.exit(setwd(old_wd), add = TRUE)
#
#   cache_dir <- ".test_tima_return_value"
#   cache_path <- fs::path_home(cache_dir)
#
#   # Should return invisibly
#   result <- withVisible(go_to_cache(dir = cache_dir))
#
#   expect_false(result$visible) # Invisible return
#   expect_equal(result$value, cache_path)
#
#   # Clean up
#   setwd(old_wd)
#   unlink(cache_path, recursive = TRUE)
# })
#
# # Error Handling Tests ----
#
# test_that("go_to_cache handles directory creation errors", {
#   skip_on_os("windows") # Permission handling different on Windows
#
#   # Try to create in a location that should fail
#   # (This is hard to test portably, skip in most cases)
#   skip("Requires specific permission setup to test directory creation failure")
# })
#
# test_that("go_to_cache provides informative error messages", {
#   # Test error message quality
#   expect_error(
#     go_to_cache(dir = NULL),
#     "NULL"
#   )
#
#   expect_error(
#     go_to_cache(dir = 123),
#     "numeric|integer"
#   )
# })
#
# # Edge Cases ----
#
# test_that("go_to_cache handles special characters in directory names", {
#   old_wd <- getwd()
#   on.exit(setwd(old_wd), add = TRUE)
#
#   # Test with hyphens and underscores
#   cache_dir <- ".test-cache_with-chars"
#   cache_path <- fs::path_home(cache_dir)
#
#   expect_invisible(go_to_cache(dir = cache_dir))
#   expect_true(dir.exists(cache_path))
#
#   # Clean up
#   setwd(old_wd)
#   unlink(cache_path, recursive = TRUE)
# })
#
# test_that("go_to_cache handles repeated calls", {
#   old_wd <- getwd()
#   on.exit(setwd(old_wd), add = TRUE)
#
#   cache_dir <- ".test_tima_repeated"
#   cache_path <- fs::path_home(cache_dir)
#
#   # First call
#   go_to_cache(dir = cache_dir)
#   expect_equal(getwd(), cache_path)
#
#   # Second call should work
#   expect_invisible(go_to_cache(dir = cache_dir))
#   expect_equal(getwd(), cache_path)
#
#   # Third call should also work
#   expect_invisible(go_to_cache(dir = cache_dir))
#   expect_equal(getwd(), cache_path)
#
#   # Clean up
#   setwd(old_wd)
#   unlink(cache_path, recursive = TRUE)
# })
#
# test_that("go_to_cache handles whitespace in directory names", {
#   old_wd <- getwd()
#   on.exit(setwd(old_wd), add = TRUE)
#
#   # Leading/trailing whitespace should be preserved
#   cache_dir <- ".test_cache_spaces"
#   cache_path <- fs::path_home(cache_dir)
#
#   expect_invisible(go_to_cache(dir = cache_dir))
#   expect_true(dir.exists(cache_path))
#
#   # Clean up
#   setwd(old_wd)
#   unlink(cache_path, recursive = TRUE)
# })
#
# test_that("go_to_cache handles directory names with dots", {
#   old_wd <- getwd()
#   on.exit(setwd(old_wd), add = TRUE)
#
#   # Multiple dots in name
#   cache_dir <- ".test.tima.cache"
#   cache_path <- fs::path_home(cache_dir)
#
#   expect_invisible(go_to_cache(dir = cache_dir))
#   expect_true(dir.exists(cache_path))
#
#   # Clean up
#   setwd(old_wd)
#   unlink(cache_path, recursive = TRUE)
# })
#
# # Integration Tests ----
#
# test_that("go_to_cache integrates with default parameter", {
#   old_wd <- getwd()
#   on.exit(setwd(old_wd), add = TRUE)
#
#   # Should use ".tima" by default
#   result <- go_to_cache()
#
#   expect_equal(getwd(), fs::path_home(".tima"))
#   expect_true(dir.exists(fs::path_home(".tima")))
#
#   # Clean up
#   setwd(old_wd)
#   # Don't remove default cache as it may be used by real workflow
# })
#
# test_that("go_to_cache works with file operations", {
#   old_wd <- getwd()
#   on.exit(setwd(old_wd), add = TRUE)
#
#   cache_dir <- ".test_tima_file_ops"
#   cache_path <- fs::path_home(cache_dir)
#
#   go_to_cache(dir = cache_dir)
#
#   # Should be able to create files in cache
#   test_file <- "test_file.txt"
#   writeLines("test content", test_file)
#
#   expect_true(file.exists(test_file))
#   expect_equal(getwd(), cache_path)
#
#   # Clean up
#   setwd(old_wd)
#   unlink(cache_path, recursive = TRUE)
# })
#
# # Performance Tests ----
#
# test_that("go_to_cache is fast", {
#   old_wd <- getwd()
#   on.exit(setwd(old_wd), add = TRUE)
#
#   cache_dir <- ".test_tima_performance"
#
#   # Should complete quickly even on first creation
#   elapsed <- system.time({
#     go_to_cache(dir = cache_dir)
#   })
#
#   expect_true(elapsed["elapsed"] < 1.0) # Should be nearly instantaneous
#
#   # Clean up
#   setwd(old_wd)
#   unlink(fs::path_home(cache_dir), recursive = TRUE)
# })
#
# test_that("go_to_cache is fast on repeated calls", {
#   old_wd <- getwd()
#   on.exit(setwd(old_wd), add = TRUE)
#
#   cache_dir <- ".test_tima_perf_repeated"
#
#   # First call
#   go_to_cache(dir = cache_dir)
#
#   # Repeated calls should be even faster
#   elapsed <- system.time({
#     for (i in 1:10) {
#       go_to_cache(dir = cache_dir)
#     }
#   })
#
#   expect_true(elapsed["elapsed"] < 1.0) # 10 calls in < 1 second
#
#   # Clean up
#   setwd(old_wd)
#   unlink(fs::path_home(cache_dir), recursive = TRUE)
# })
#
# # Regression Tests ----
#
# test_that("go_to_cache maintains backward compatibility", {
#   # Function signature should have one parameter with default
#   params <- formals(go_to_cache)
#
#   expect_equal(length(params), 1)
#   expect_true("dir" %in% names(params))
#   expect_equal(params$dir, ".tima")
# })
#
# test_that("go_to_cache return value is character", {
#   old_wd <- getwd()
#   on.exit(setwd(old_wd), add = TRUE)
#
#   result <- go_to_cache(dir = ".test_tima_return_type")
#
#   expect_type(result, "character")
#   expect_equal(length(result), 1)
#
#   # Clean up
#   setwd(old_wd)
#   unlink(result, recursive = TRUE)
# })
#
# # Documentation Tests ----
#
# test_that("go_to_cache has proper documentation", {
#   help_file <- utils::help("go_to_cache", package = "tima")
#   expect_true(length(help_file) > 0)
# })
#
# # Side Effects Tests ----
#
# test_that("go_to_cache side effect is working directory change", {
#   old_wd <- getwd()
#   on.exit(setwd(old_wd), add = TRUE)
#
#   cache_dir <- ".test_tima_side_effect"
#
#   # Working directory should change
#   go_to_cache(dir = cache_dir)
#   new_wd <- getwd()
#
#   expect_false(new_wd == old_wd)
#   expect_equal(new_wd, fs::path_home(cache_dir))
#
#   # Clean up
#   setwd(old_wd)
#   unlink(fs::path_home(cache_dir), recursive = TRUE)
# })
#
# test_that("go_to_cache doesn't create extra files or directories", {
#   old_wd <- getwd()
#   on.exit(setwd(old_wd), add = TRUE)
#
#   cache_dir <- ".test_tima_no_extras"
#   cache_path <- fs::path_home(cache_dir)
#
#   # Remove if exists
#   if (dir.exists(cache_path)) {
#     unlink(cache_path, recursive = TRUE)
#   }
#
#   go_to_cache(dir = cache_dir)
#
#   # Should only contain what was explicitly created
#   contents <- list.files(cache_path, all.files = TRUE, no.. = TRUE)
#
#   # Should be empty (or only . and ..)
#   expect_equal(length(contents), 0)
#
#   # Clean up
#   setwd(old_wd)
#   unlink(cache_path, recursive = TRUE)
# })
#
# # Path Resolution Tests ----
#
# test_that("go_to_cache resolves path correctly", {
#   old_wd <- getwd()
#   on.exit(setwd(old_wd), add = TRUE)
#
#   cache_dir <- ".test_tima_path_resolution"
#   cache_path <- fs::path_home(cache_dir)
#
#   result <- go_to_cache(dir = cache_dir)
#
#   # Result should be absolute path
#   expect_true(fs::is_absolute_path(result))
#   expect_equal(result, cache_path)
#
#   # Clean up
#   setwd(old_wd)
#   unlink(cache_path, recursive = TRUE)
# })
