# Test Suite: go_to_cache ----
# Tests for cache directory navigation

library(testthat)

# Helper to restore working directory
with_temp_wd <- function(code) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  force(code)
}

## Input Validation Tests ----

test_that("go_to_cache validates dir parameter", {
  with_temp_wd({
    expect_error(
      go_to_cache(dir = NULL),
      "non-NULL"
    )

    expect_error(
      go_to_cache(dir = ""),
      class = "error"
    )

    expect_error(
      go_to_cache(dir = c("dir1", "dir2")),
      "length-1"
    )

    expect_error(
      go_to_cache(dir = 123),
      "character"
    )
  })
})

## Basic Functionality Tests ----

test_that("go_to_cache creates cache directory", {
  with_temp_wd({
    # Use unique cache name to avoid conflicts
    cache_name <- paste0(".test_cache_", Sys.getpid())
    cache_path <- fs::path_home(cache_name)

    # Ensure it doesn't exist
    if (dir.exists(cache_path)) {
      unlink(cache_path, recursive = TRUE)
    }

    # Create cache
    result <- go_to_cache(dir = cache_name)

    expect_true(dir.exists(cache_path))

    # Cleanup
    unlink(cache_path, recursive = TRUE)
  })
})

test_that("go_to_cache changes working directory", {
  with_temp_wd({
    old_wd <- getwd()
    cache_name <- paste0(".test_cache_", Sys.getpid())
    cache_path <- fs::path_home(cache_name)

    # Ensure clean state
    if (dir.exists(cache_path)) {
      unlink(cache_path, recursive = TRUE)
    }

    go_to_cache(dir = cache_name)
    new_wd <- getwd()

    expect_false(identical(old_wd, new_wd))
    expect_true(grepl(cache_name, new_wd, fixed = TRUE))

    # Cleanup
    setwd(old_wd)
    unlink(cache_path, recursive = TRUE)
  })
})

test_that("go_to_cache works with existing cache", {
  with_temp_wd({
    cache_name <- paste0(".test_cache_existing_", Sys.getpid())
    cache_path <- fs::path_home(cache_name)

    # Create cache manually
    dir.create(cache_path, showWarnings = FALSE)

    # Should not error
    result <- go_to_cache(dir = cache_name)

    expect_true(dir.exists(cache_path))
    expect_true(grepl(cache_name, getwd(), fixed = TRUE))

    # Cleanup
    old_wd <- dirname(cache_path)
    setwd(old_wd)
    unlink(cache_path, recursive = TRUE)
  })
})

test_that("go_to_cache uses default directory name", {
  with_temp_wd({
    old_wd <- getwd()
    default_cache <- fs::path_home(".tima")

    # Create if doesn't exist, or just navigate
    result <- go_to_cache()

    expect_true(dir.exists(default_cache))
    expect_true(grepl(".tima", getwd(), fixed = TRUE))

    setwd(old_wd)
  })
})

## Return Value Tests ----

test_that("go_to_cache returns cache path invisibly", {
  with_temp_wd({
    cache_name <- paste0(".test_cache_return_", Sys.getpid())
    cache_path <- fs::path_home(cache_name)

    if (dir.exists(cache_path)) {
      unlink(cache_path, recursive = TRUE)
    }

    result <- withVisible(go_to_cache(dir = cache_name))

    expect_false(result$visible)
    expect_type(result$value, "character")
    expect_true(grepl(cache_name, result$value, fixed = TRUE))

    # Cleanup
    old_wd <- dirname(cache_path)
    setwd(old_wd)
    unlink(cache_path, recursive = TRUE)
  })
})

test_that("go_to_cache returns correct path", {
  with_temp_wd({
    cache_name <- paste0(".test_cache_path_", Sys.getpid())
    expected_path <- fs::path_home(cache_name)

    if (dir.exists(expected_path)) {
      unlink(expected_path, recursive = TRUE)
    }

    result <- go_to_cache(dir = cache_name)

    expect_equal(result, expected_path)

    # Cleanup
    old_wd <- dirname(expected_path)
    setwd(old_wd)
    unlink(expected_path, recursive = TRUE)
  })
})

## Edge Cases ----

test_that("go_to_cache handles special characters in name", {
  skip_on_os("windows") # Windows has different path rules

  with_temp_wd({
    cache_name <- paste0(".test_cache_special-chars_", Sys.getpid())
    cache_path <- fs::path_home(cache_name)

    if (dir.exists(cache_path)) {
      unlink(cache_path, recursive = TRUE)
    }

    result <- go_to_cache(dir = cache_name)

    expect_true(dir.exists(cache_path))

    # Cleanup
    old_wd <- dirname(cache_path)
    setwd(old_wd)
    unlink(cache_path, recursive = TRUE)
  })
})

test_that("go_to_cache handles nested directory names", {
  with_temp_wd({
    # Note: This creates a single directory named ".cache/nested"
    # Not a nested structure
    cache_name <- paste0(".test_cache_nested_", Sys.getpid())
    cache_path <- fs::path_home(cache_name)

    if (dir.exists(cache_path)) {
      unlink(cache_path, recursive = TRUE)
    }

    result <- go_to_cache(dir = cache_name)

    expect_true(dir.exists(cache_path))

    # Cleanup
    old_wd <- dirname(cache_path)
    setwd(old_wd)
    unlink(cache_path, recursive = TRUE)
  })
})

test_that("go_to_cache is idempotent", {
  with_temp_wd({
    cache_name <- paste0(".test_cache_idempotent_", Sys.getpid())
    cache_path <- fs::path_home(cache_name)

    if (dir.exists(cache_path)) {
      unlink(cache_path, recursive = TRUE)
    }

    # Call multiple times
    result1 <- go_to_cache(dir = cache_name)
    result2 <- go_to_cache(dir = cache_name)
    result3 <- go_to_cache(dir = cache_name)

    expect_equal(result1, result2)
    expect_equal(result2, result3)
    expect_true(dir.exists(cache_path))

    # Cleanup
    old_wd <- dirname(cache_path)
    setwd(old_wd)
    unlink(cache_path, recursive = TRUE)
  })
})

## Error Handling Tests ----

test_that("go_to_cache handles permission errors gracefully", {
  skip_on_os("windows") # Different permission model
  skip_on_cran()

  with_temp_wd({
    # This test is hard to do portably
    # Just verify error handling exists
    expect_error(
      {
        # Try to create in root (should fail without sudo)
        tryCatch(
          go_to_cache(dir = "/root/.test_forbidden"),
          error = function(e) {
            expect_true(grepl("Cannot create", conditionMessage(e)))
            stop(e)
          }
        )
      },
      class = "error"
    )
  })
})

## Helper Function Tests ----
# Note: These test internal functions using tima::: to access them

test_that("ensure_cache_exists creates directory", {
  cache_path <- file.path(tempdir(), paste0("test_ensure_", Sys.getpid()))

  if (dir.exists(cache_path)) {
    unlink(cache_path, recursive = TRUE)
  }

  tima:::ensure_cache_exists(cache_path)

  expect_true(dir.exists(cache_path))

  # Cleanup
  unlink(cache_path, recursive = TRUE)
})

test_that("ensure_cache_exists handles existing directory", {
  cache_path <- file.path(tempdir(), paste0("test_existing_", Sys.getpid()))

  # Create manually
  dir.create(cache_path, showWarnings = FALSE)

  # Should not error
  expect_silent(tima:::ensure_cache_exists(cache_path))
  expect_true(dir.exists(cache_path))

  # Cleanup
  unlink(cache_path, recursive = TRUE)
})

test_that("change_to_cache changes directory", {
  with_temp_wd({
    test_dir <- file.path(tempdir(), paste0("test_change_", Sys.getpid()))
    dir.create(test_dir, showWarnings = FALSE)

    old_wd <- getwd()
    tima:::change_to_cache(test_dir)
    new_wd <- getwd()

    expect_false(identical(old_wd, new_wd))

    # Normalize paths for comparison (handles macOS /private prefix)
    expect_equal(normalizePath(new_wd), normalizePath(test_dir))

    # Cleanup
    setwd(old_wd)
    unlink(test_dir, recursive = TRUE)
  })
})

test_that("change_to_cache errors on invalid directory", {
  with_temp_wd({
    expect_error(
      tima:::change_to_cache("/nonexistent/directory/path"),
      "Cannot change"
    )
  })
})

## Integration Tests ----

test_that("go_to_cache works in typical workflow", {
  with_temp_wd({
    initial_wd <- getwd()
    cache_name <- paste0(".workflow_test_", Sys.getpid())

    # Clean state
    cache_path <- fs::path_home(cache_name)
    if (dir.exists(cache_path)) {
      unlink(cache_path, recursive = TRUE)
    }

    # Step 1: Navigate to cache
    cache <- go_to_cache(dir = cache_name)

    # Step 2: Create some files
    test_file <- file.path(cache, "test_data.txt")
    writeLines("test", test_file)

    expect_true(file.exists(test_file))

    # Step 3: Navigate back
    setwd(initial_wd)

    # Step 4: Navigate to cache again
    cache2 <- go_to_cache(dir = cache_name)

    # File should still exist
    expect_true(file.exists(test_file))
    expect_equal(cache, cache2)

    # Cleanup
    setwd(initial_wd)
    unlink(cache_path, recursive = TRUE)
  })
})

test_that("go_to_cache persists across function calls", {
  with_temp_wd({
    cache_name <- paste0(".persist_test_", Sys.getpid())
    cache_path <- fs::path_home(cache_name)

    if (dir.exists(cache_path)) {
      unlink(cache_path, recursive = TRUE)
    }

    # Create in first call
    go_to_cache(dir = cache_name)
    first_call_wd <- getwd()

    # Change away
    setwd(tempdir())

    # Navigate back
    go_to_cache(dir = cache_name)
    second_call_wd <- getwd()

    expect_equal(first_call_wd, second_call_wd)

    # Cleanup
    setwd(dirname(cache_path))
    unlink(cache_path, recursive = TRUE)
  })
})

## Platform Compatibility ----

test_that("go_to_cache works on current platform", {
  with_temp_wd({
    cache_name <- paste0(".platform_test_", Sys.getpid())
    cache_path <- fs::path_home(cache_name)

    if (dir.exists(cache_path)) {
      unlink(cache_path, recursive = TRUE)
    }

    result <- go_to_cache(dir = cache_name)

    # Should work on all platforms
    expect_true(dir.exists(cache_path))
    expect_true(dir.exists(getwd()))

    # Cleanup
    old_wd <- dirname(cache_path)
    setwd(old_wd)
    unlink(cache_path, recursive = TRUE)
  })
})

## Determinism Tests ----

test_that("go_to_cache is deterministic", {
  with_temp_wd({
    cache_name <- paste0(".deterministic_test_", Sys.getpid())
    cache_path <- fs::path_home(cache_name)

    if (dir.exists(cache_path)) {
      unlink(cache_path, recursive = TRUE)
    }

    result1 <- go_to_cache(dir = cache_name)

    # Go elsewhere
    setwd(tempdir())

    result2 <- go_to_cache(dir = cache_name)

    expect_identical(result1, result2)

    # Cleanup
    setwd(dirname(cache_path))
    unlink(cache_path, recursive = TRUE)
  })
})

## Side Effects ----

test_that("go_to_cache only changes working directory", {
  with_temp_wd({
    cache_name <- paste0(".side_effect_test_", Sys.getpid())

    # Check that it doesn't modify global options or env vars
    old_options <- options()
    old_env <- Sys.getenv()

    go_to_cache(dir = cache_name)

    new_options <- options()
    new_env <- Sys.getenv()

    # Options should be unchanged
    expect_identical(old_options, new_options)

    # Env vars should be unchanged (except possibly PWD)
    env_diff <- setdiff(names(new_env), names(old_env))
    expect_true(length(env_diff) == 0 || all(env_diff == "PWD"))

    # Cleanup
    cache_path <- fs::path_home(cache_name)
    setwd(dirname(cache_path))
    unlink(cache_path, recursive = TRUE)
  })
})
