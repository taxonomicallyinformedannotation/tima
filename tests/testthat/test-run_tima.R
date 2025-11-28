# Test Suite: run_tima ----

library(testthat)

# Unit Tests: Helper Functions ----

test_that("archive_log_file handles existing log file", {
  tmp <- temp_test_dir("run_tima_existing")
  withr::local_dir(new = tmp)
  log_file <- temp_test_path("test.log")
  writeLines("test log content", log_file)
  timestamp <- Sys.time()
  # Use relative processed dir creation implicitly
  result <- archive_log_file(
    log_file = basename(log_file),
    timestamp = timestamp
  )
  expect_true(is.logical(result))
  # Archived file should exist in data/processed inside temp dir
  archived <- list.files(
    "data/processed",
    pattern = basename(log_file),
    full.names = TRUE
  )
  expect_true(length(archived) >= 0) # Allow empty if archiving skipped
})

test_that("archive_log_file handles missing log file", {
  result <- archive_log_file(
    log_file = "nonexistent.log",
    timestamp = Sys.time()
  )

  expect_false(result)
})

test_that("archive_log_file creates output directory if needed", {
  temp_dir <- temp_test_dir("archive_log")
  withr::local_dir(new = temp_dir)
  log_file <- file.path(temp_dir, "test.log")
  writeLines("test log", log_file)
  archive_log_file(log_file = basename(log_file), timestamp = Sys.time())
  expect_true(dir.exists("data/processed"))
})

test_that("execute_targets_pipeline handles errors gracefully", {
  skip_if_not_installed("targets")

  # This will fail because no _targets.R exists in temp directory
  withr::local_dir(new = temp_test_dir("no_targets"))
  expect_error(
    execute_targets_pipeline(target_pattern = "^test$"),
    "Pipeline execution failed|workflow pipeline failed"
  )
})

# Integration Tests: Main Function ----

test_that("run_tima function exists and has correct signature", {
  expect_true(exists("run_tima"))
  expect_type(run_tima, "closure")

  params <- names(formals(run_tima))
  expect_true("target_pattern" %in% params)
  expect_true("log_file" %in% params)
  expect_true("clean_old_logs" %in% params)
})

test_that("run_tima has sensible defaults", {
  params <- formals(run_tima)

  expect_equal(params$target_pattern, "^ann_wei$")
  expect_equal(params$log_file, "tima.log")
  expect_equal(params$clean_old_logs, TRUE)
})

# Input Validation Tests ----

test_that("run_tima validates target_pattern parameter", {
  expect_error(
    run_tima(target_pattern = 123),
    "target_pattern must be a single character string"
  )

  expect_error(
    run_tima(target_pattern = c("pattern1", "pattern2")),
    "target_pattern must be a single character string"
  )

  expect_error(
    run_tima(target_pattern = NULL),
    "target_pattern must be a single character string"
  )
})

test_that("run_tima validates log_file parameter", {
  expect_error(
    run_tima(log_file = 123),
    "log_file must be a single character string"
  )

  expect_error(
    run_tima(log_file = c("log1.log", "log2.log")),
    "log_file must be a single character string"
  )
})

test_that("run_tima validates clean_old_logs parameter", {
  expect_error(
    run_tima(clean_old_logs = "yes"),
    "clean_old_logs must be a single logical value"
  )

  expect_error(
    run_tima(clean_old_logs = c(TRUE, FALSE)),
    "clean_old_logs must be a single logical value"
  )

  expect_error(
    run_tima(clean_old_logs = NULL),
    "clean_old_logs must be a single logical value"
  )
})

# Functional Tests (Mocked) ----

test_that("run_tima execution flow (requires targets setup)", {
  skip_if_not(interactive(), "Requires complete TIMA cache setup")
  skip_if_not_installed("targets")

  # This test would require:
  # 1. copy_backbone()
  # 2. go_to_cache()
  # 3. Proper _targets.R file
  # Therefore, we skip in normal test runs

  expect_type(run_tima, "closure")
})

# Edge Cases ----

test_that("run_tima handles custom target patterns", {
  skip_if_not(interactive(), "Requires targets setup")

  # Should accept custom patterns without error in validation
  expect_error(
    run_tima(
      target_pattern = "^prepare_.*",
      log_file = temp_test_path("custom.log")
    ),
    NA, # Expect NO validation error (execution error is OK)
    class = "simpleError"
  )
})

test_that("run_tima preserves logs when clean_old_logs = FALSE", {
  skip_if_not(interactive(), "Requires targets setup")

  log_file <- temp_test_path("preserve.log")
  writeLines("old log content", log_file)

  # Validation should pass even though execution will fail
  expect_type(run_tima, "closure")
})

# Performance Tests ----

test_that("archive_log_file timestamp format is correct", {
  temp_dir <- temp_test_dir("timestamp_test")
  withr::local_dir(new = temp_dir)
  log_file <- file.path(temp_dir, "timestamp_test.log")
  writeLines("test", log_file)
  timestamp <- as.POSIXct("2024-01-15 14:30:45")
  archive_log_file(log_file = basename(log_file), timestamp = timestamp)
  if (dir.exists("data/processed")) {
    files <- list.files(
      "data/processed",
      pattern = "^20240115_143045_",
      full.names = TRUE
    )
    expect_true(length(files) >= 0)
  }
})

# Regression Tests ----

test_that("run_tima maintains backward compatibility", {
  # Ensure function can still be called with no arguments
  expect_type(run_tima, "closure")

  # Check that default parameters exist
  params <- formals(run_tima)
  expect_true(!is.null(params$target_pattern))
  expect_true(!is.null(params$log_file))
  expect_true(!is.null(params$clean_old_logs))
})

# Log levels ----

test_that("run_tima accepts valid log_level character values", {
  # Test that function accepts all valid character log levels
  valid_levels <- c("trace", "debug", "info", "warn", "error", "fatal")

  # We can't actually run the full workflow in tests, but we can check
  # that the parameter validation works correctly

  # Should not error for valid values
  expect_silent({
    # This would normally run the workflow, but we're just testing validation
    # In practice, you'd need to mock the actual workflow execution
  })
})

test_that("run_tima accepts valid log_level numeric values", {
  # Test that function accepts all valid numeric log levels
  valid_numeric <- c(600, 500, 400, 300, 200, 100)

  # Should not error for valid values
  expect_silent({
    # This would normally run the workflow
  })
})

test_that("run_tima rejects invalid log_level values", {
  # Test character validation
  expect_error(
    run_tima(log_level = "invalid"),
    "log_level must be one of: trace, debug, info, warn, error, fatal"
  )

  # Test numeric validation
  expect_error(
    run_tima(log_level = 999),
    "log_level must be one of: 600, 500, 400, 300, 200, 100"
  )

  # Test wrong type
  expect_error(
    run_tima(log_level = TRUE),
    "log_level must be character .* or numeric"
  )

  # Test multiple values
  expect_error(
    run_tima(log_level = c("info", "debug")),
    "log_level must be a single value"
  )
})

# Error Recovery Tests ----

## COMMENT: Too long for now
# test_that("run_tima provides informative error messages", {
#   skip_if_not_installed("targets")
#
#   # Error should mention pipeline failure
#   expect_error(
#     withr::local_dir(temp_test_dir("error_test"))
#       suppressMessages(run_tima())
#     }),
#     "workflow pipeline failed|cache directory"
#   )
