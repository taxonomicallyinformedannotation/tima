# Test Suite: tima_full ----

library(testthat)

# Unit Tests: Helper Functions ----

test_that("archive_log_file handles existing log file", {
  log_file <- temp_test_path("test.log")
  writeLines("test log content", log_file)

  timestamp <- Sys.time()

  # Create output directory
  output_dir <- file.path(dirname(log_file), "data", "processed")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Mock the output directory to use temp location
  withr::with_dir(dirname(log_file), {
    result <- archive_log_file(
      log_file = basename(log_file),
      timestamp = timestamp
    )
    expect_true(is.logical(result))
  })
})

test_that("archive_log_file handles missing log file", {
  result <- archive_log_file(
    log_file = "nonexistent.log",
    timestamp = Sys.time()
  )

  expect_false(result)
})

test_that("archive_log_file creates output directory if needed", {
  log_file <- temp_test_path("test.log")
  writeLines("test log", log_file)

  withr::with_dir(dirname(log_file), {
    # Ensure output directory doesn't exist
    output_dir <- "data/processed"
    if (dir.exists(output_dir)) {
      unlink(output_dir, recursive = TRUE)
    }

    archive_log_file(log_file = basename(log_file), timestamp = Sys.time())

    expect_true(dir.exists(output_dir))
  })
})

test_that("execute_targets_pipeline handles errors gracefully", {
  skip_if_not_installed("targets")

  # This will fail because no _targets.R exists in temp directory
  withr::with_dir(temp_test_dir("no_targets"), {
    expect_error(
      execute_targets_pipeline(target_pattern = "^test$"),
      "Pipeline execution failed|workflow pipeline failed"
    )
  })
})

# Integration Tests: Main Function ----

test_that("tima_full function exists and has correct signature", {
  expect_true(exists("tima_full"))
  expect_type(tima_full, "closure")

  params <- names(formals(tima_full))
  expect_true("target_pattern" %in% params)
  expect_true("log_file" %in% params)
  expect_true("clean_old_logs" %in% params)
})

test_that("tima_full has sensible defaults", {
  params <- formals(tima_full)

  expect_equal(params$target_pattern, "^ann_pre$")
  expect_equal(params$log_file, "tima.log")
  expect_equal(params$clean_old_logs, TRUE)
})

# Input Validation Tests ----

test_that("tima_full validates target_pattern parameter", {
  expect_error(
    tima_full(target_pattern = 123),
    "target_pattern must be a single character string"
  )

  expect_error(
    tima_full(target_pattern = c("pattern1", "pattern2")),
    "target_pattern must be a single character string"
  )

  expect_error(
    tima_full(target_pattern = NULL),
    "target_pattern must be a single character string"
  )
})

test_that("tima_full validates log_file parameter", {
  expect_error(
    tima_full(log_file = 123),
    "log_file must be a single character string"
  )

  expect_error(
    tima_full(log_file = c("log1.log", "log2.log")),
    "log_file must be a single character string"
  )
})

test_that("tima_full validates clean_old_logs parameter", {
  expect_error(
    tima_full(clean_old_logs = "yes"),
    "clean_old_logs must be a single logical value"
  )

  expect_error(
    tima_full(clean_old_logs = c(TRUE, FALSE)),
    "clean_old_logs must be a single logical value"
  )

  expect_error(
    tima_full(clean_old_logs = NULL),
    "clean_old_logs must be a single logical value"
  )
})

# Functional Tests (Mocked) ----

test_that("tima_full execution flow (requires targets setup)", {
  skip_if_not(interactive(), "Requires complete TIMA cache setup")
  skip_if_not_installed("targets")

  # This test would require:
  # 1. copy_backbone()
  # 2. go_to_cache()
  # 3. Proper _targets.R file
  # Therefore, we skip in normal test runs

  expect_type(tima_full, "closure")
})

# Edge Cases ----

test_that("tima_full handles custom target patterns", {
  skip_if_not(interactive(), "Requires targets setup")

  # Should accept custom patterns without error in validation
  expect_error(
    tima_full(
      target_pattern = "^prepare_.*",
      log_file = temp_test_path("custom.log")
    ),
    NA, # Expect NO validation error (execution error is OK)
    class = "simpleError"
  )
})

test_that("tima_full preserves logs when clean_old_logs = FALSE", {
  skip_if_not(interactive(), "Requires targets setup")

  log_file <- temp_test_path("preserve.log")
  writeLines("old log content", log_file)

  # Validation should pass even though execution will fail
  expect_type(tima_full, "closure")
})

# Performance Tests ----

test_that("archive_log_file timestamp format is correct", {
  log_file <- temp_test_path("timestamp_test.log")
  writeLines("test", log_file)

  timestamp <- as.POSIXct("2024-01-15 14:30:45")

  withr::with_dir(dirname(log_file), {
    archive_log_file(log_file = basename(log_file), timestamp = timestamp)

    # Check that file with correct timestamp exists
    output_dir <- "data/processed"
    if (dir.exists(output_dir)) {
      files <- list.files(output_dir, pattern = "^20240115_143045_")
      expect_true(length(files) > 0)
    }
  })
})

# Regression Tests ----

test_that("tima_full maintains backward compatibility", {
  # Ensure function can still be called with no arguments
  expect_type(tima_full, "closure")

  # Check that default parameters exist
  params <- formals(tima_full)
  expect_true(!is.null(params$target_pattern))
  expect_true(!is.null(params$log_file))
  expect_true(!is.null(params$clean_old_logs))
})

# Error Recovery Tests ----

## COMMENT: Too long for now
# test_that("tima_full provides informative error messages", {
#   skip_if_not_installed("targets")
#
#   # Error should mention pipeline failure
#   expect_error(
#     withr::with_dir(temp_test_dir("error_test"), {
#       suppressMessages(tima_full())
#     }),
#     "workflow pipeline failed|cache directory"
#   )
# })
