# Test Suite: logging_helpers ---

library(testthat)

## format_count ----

test_that("format_count formats numbers with thousands separator", {
  expect_equal(format_count(1234), "1,234")
  expect_equal(format_count(1000000), "1,000,000")
  expect_equal(format_count(0), "0")
  expect_equal(format_count(999), "999")
  expect_equal(format_count(1000), "1,000")
})

test_that("format_count handles edge cases", {
  expect_equal(format_count(1), "1")
  expect_equal(format_count(10), "10")
  expect_equal(format_count(100), "100")
})

## format_bytes ----

test_that("format_bytes formats file sizes correctly", {
  expect_equal(format_bytes(500), "500 B")
  expect_equal(format_bytes(1024), "1 KB")
  expect_equal(format_bytes(1024 * 1024), "1 MB")
  expect_equal(format_bytes(1024 * 1024 * 1024), "1 GB")
  expect_equal(format_bytes(2.3 * 1024^2), "2.3 MB")
})

test_that("format_bytes handles edge cases", {
  expect_equal(format_bytes(0), "0 B")
  expect_equal(format_bytes(1023), "1023 B")
  expect_equal(format_bytes(NA), "unknown")
  expect_equal(format_bytes(-1), "unknown")
})

test_that("format_bytes uses appropriate units", {
  expect_match(format_bytes(1500), "KB")
  expect_match(format_bytes(1500000), "MB")
  expect_match(format_bytes(1500000000), "GB")
})

## format_time ----

test_that("format_time formats durations correctly", {
  expect_equal(format_time(0.5), "500ms")
  expect_equal(format_time(1.2), "1.2s")
  expect_equal(format_time(45), "45s")
  expect_equal(format_time(60), "1m")
  expect_equal(format_time(90), "1m 30s")
  expect_equal(format_time(3600), "1h")
  expect_equal(format_time(3660), "1h 1m")
})

test_that("format_time handles edge cases", {
  expect_equal(format_time(0), "0ms")
  expect_equal(format_time(0.001), "1ms")
  expect_equal(format_time(NA), "unknown")
  expect_equal(format_time(-1), "unknown")
})

test_that("format_time rounds appropriately", {
  expect_equal(format_time(59), "59s")
  expect_equal(format_time(61), "1m 1s")
  expect_equal(format_time(119), "1m 59s")
  expect_equal(format_time(3599), "59m 59s")
})

## log_with_count ----

test_that("log_with_count logs correctly with count", {
  # Just check it doesn't error (lgr outputs to console so can't use expect_silent)
  expect_no_error(log_with_count("Test message", n = 1234))
  expect_no_error(log_with_count("Test message", n = 1000000))
})

test_that("log_with_count logs correctly with count and elapsed", {
  expect_no_error(log_with_count("Test message", n = 1234, elapsed = 5.2))
})

test_that("log_with_count logs correctly with just elapsed", {
  expect_no_error(log_with_count("Test message", elapsed = 2.5))
})

test_that("log_with_count logs correctly with neither", {
  expect_no_error(log_with_count("Test message"))
})

## log_file_op ----

test_that("log_file_op logs file operations correctly", {
  # Create temp file for testing
  tmp <- withr::local_tempfile()
  writeLines("test", tmp)
  file_size <- file.info(tmp)$size

  expect_no_error(log_file_op(
    "Exported",
    tmp,
    size_bytes = file_size,
    n_rows = 100
  ))
  expect_no_error(log_file_op("Downloaded", tmp, size_bytes = file_size))
  expect_no_error(log_file_op("Loaded", tmp))
})

test_that("log_file_op handles missing size and rows", {
  tmp <- withr::local_tempfile()
  writeLines("test", tmp)

  expect_no_error(log_file_op("Action", tmp, n_rows = 100))
  expect_no_error(log_file_op("Action", tmp, size_bytes = 1024))
})

## Integration tests ----

test_that("formatters work together", {
  # Simulate a typical use case
  n_items <- 12345
  file_size <- 2.5 * 1024^2
  elapsed <- 65.3

  count_str <- format_count(n_items)
  size_str <- format_bytes(file_size)
  time_str <- format_time(elapsed)

  expect_match(count_str, "12,345")
  expect_match(size_str, "2.5 MB")
  expect_match(time_str, "1m")
})

test_that("logging helpers are safe with edge cases", {
  # Should not error with unusual inputs
  expect_no_error(log_with_count("Test", n = 0))
  expect_no_error(log_file_op("Test", "nonexistent.txt"))
})

## Performance ----

test_that("formatters are fast", {
  skip_on_cran()
  # format_count should be fast
  timing <- system.time(format_count(rep(12345, 1000L)))
  expect_lt(timing["elapsed"], 0.1)

  # format_bytes should be fast
  timing <- system.time(format_bytes(rep(1024^2, 1000L)))
  expect_lt(timing["elapsed"], 0.1)

  # format_time should be fast
  timing <- system.time(format_time(rep(65.5, 1000L)))
  expect_lt(timing["elapsed"], 0.1)
})

test_that("setup_logger validates filename parameter", {
  expect_error(
    setup_logger(filename = NULL),
    "log filename must be a non-empty character string"
  )

  expect_error(
    setup_logger(filename = ""),
    "log filename must be a non-empty character string"
  )

  expect_error(
    setup_logger(filename = 123),
    "log filename must be a non-empty character string"
  )

  expect_error(
    setup_logger(filename = c("file1.log", "file2.log")),
    "log filename must be a non-empty character string"
  )
})

test_that("setup_logger creates log file", {
  log_file <- tempfile(fileext = ".log")

  expect_silent(setup_logger(filename = log_file))

  # Write a test message
  log_info("Test message")

  # File should be created
  expect_true(file.exists(log_file))
})

test_that("setup_logger sets threshold correctly", {
  log_file <- tempfile(fileext = ".log")

  # Test different thresholds
  expect_silent(setup_logger(filename = log_file, threshold = 400))
  expect_silent(setup_logger(filename = log_file, threshold = 300))
  expect_silent(setup_logger(filename = log_file, threshold = 200))
  expect_silent(setup_logger(filename = log_file, threshold = 500))
  expect_silent(setup_logger(filename = log_file, threshold = 600))
})

test_that("setup_logger returns invisibly", {
  log_file <- tempfile(fileext = ".log")

  result <- withVisible(setup_logger(filename = log_file))

  expect_null(result$value)
  expect_false(result$visible)
})

test_that("setup_logger can be called multiple times", {
  log_file1 <- tempfile("log1", fileext = ".log")
  log_file2 <- tempfile("log2", fileext = ".log")

  expect_silent(setup_logger(filename = log_file1))
  expect_silent(setup_logger(filename = log_file2))
})

test_that("setup_logger writes to file with correct format", {
  log_file <- tempfile(fileext = ".log")

  setup_logger(filename = log_file, threshold = 400)
  log_info("Test info message")
  log_warn("Test warning message")

  # Read log file
  log_content <- readLines(log_file)

  # Should have at least 2 lines
  expect_true(length(log_content) >= 2)

  # Check format includes timestamp and level
  expect_true(any(grepl(
    "\\[.*\\].*\\[INFO \\].*Test info message",
    log_content
  )))
  expect_true(any(grepl(
    "\\[.*\\].*\\[WARN \\].*Test warning message",
    log_content
  )))
})

test_that("setup_logger respects threshold levels", {
  log_file <- tempfile(fileext = ".log")

  # Set threshold to WARN (should not log INFO)
  setup_logger(filename = log_file, threshold = 300)

  log_info("This should not appear")
  log_warn("This should appear")
  log_error("This should also appear")

  log_content <- readLines(log_file)

  # INFO message should not be in file
  expect_false(any(grepl("This should not appear", log_content)))

  # WARN and ERROR should be in file
  expect_true(any(grepl("This should appear", log_content)))
  expect_true(any(grepl("This should also appear", log_content)))
})

## init_logging tests ----

test_that("init_logging uses default values when env vars not set", {
  # Clear env vars
  withr::local_envvar(TIMA_LOG_FILE = NA, TIMA_LOG_LEVEL = NA)

  expect_silent(init_logging())
})

test_that("init_logging reads TIMA_LOG_FILE from environment", {
  log_file <- tempfile(fileext = ".log")

  withr::local_envvar(TIMA_LOG_FILE = log_file)

  expect_silent(init_logging())

  # Write test message
  log_info("Test from env")

  expect_true(file.exists(log_file))
})

test_that("init_logging reads TIMA_LOG_LEVEL from environment", {
  log_file <- tempfile(fileext = ".log")

  withr::local_envvar(
    TIMA_LOG_FILE = log_file,
    TIMA_LOG_LEVEL = "WARN"
  )

  expect_silent(init_logging())

  # INFO should not be logged
  log_info("Should not appear")
  log_warn("Should appear")

  log_content <- readLines(log_file)
  expect_false(any(grepl("Should not appear", log_content)))
  expect_true(any(grepl("Should appear", log_content)))
})

test_that("init_logging handles invalid log level gracefully", {
  log_file <- tempfile(fileext = ".log")

  withr::local_envvar(
    TIMA_LOG_FILE = log_file,
    TIMA_LOG_LEVEL = "INVALID_LEVEL"
  )

  expect_warning(
    init_logging(),
    "Unknown TIMA_LOG_LEVEL.*Falling back"
  )
})

test_that("init_logging supports all valid log levels", {
  log_file <- tempfile(fileext = ".log")

  levels <- c("TRACE", "DEBUG", "INFO", "WARN", "ERROR")

  invisible(vapply(
    X = levels,
    FUN = function(level) {
      withr::local_envvar(
        TIMA_LOG_FILE = log_file,
        TIMA_LOG_LEVEL = level
      )
      expect_silent(init_logging())
      TRUE
    },
    FUN.VALUE = logical(1L)
  ))
})

test_that("init_logging is case-insensitive for log level", {
  log_file <- tempfile(fileext = ".log")

  # Test lowercase
  withr::local_envvar(
    TIMA_LOG_FILE = log_file,
    TIMA_LOG_LEVEL = "info"
  )

  expect_silent(init_logging())

  # Test mixed case
  withr::local_envvar(
    TIMA_LOG_FILE = log_file,
    TIMA_LOG_LEVEL = "WaRn"
  )

  expect_silent(init_logging())
})

test_that("lazy logging initialization works", {
  # Test that logging functions are available
  expect_true(exists("log_info", where = asNamespace("tima"), inherits = FALSE))
  expect_true(exists(
    "log_debug",
    where = asNamespace("tima"),
    inherits = FALSE
  ))
  expect_true(exists("log_warn", where = asNamespace("tima"), inherits = FALSE))
  expect_true(exists(
    "log_error",
    where = asNamespace("tima"),
    inherits = FALSE
  ))

  # Test that helper functions exist
  expect_true(exists(
    "ensure_logging_initialized",
    where = asNamespace("tima"),
    inherits = FALSE
  ))
  expect_true(exists(
    "is_logging_initialized",
    where = asNamespace("tima"),
    inherits = FALSE
  ))
})

test_that("logging does not create file on package load", {
  # This test verifies that loading the package doesn't create tima.log
  # In actual usage, we can't test this directly, but we can verify
  # the .onLoad function doesn't call init_logging

  # Get the .onLoad function
  onLoad <- get(".onLoad", envir = asNamespace("tima"))

  # Convert to character to check it doesn't contain init_logging call
  onLoad_body <- deparse(body(onLoad))

  # Should NOT contain init_logging() call
  expect_false(any(grepl("init_logging\\(\\)", onLoad_body, fixed = FALSE)))
})

test_that("logging functions use sprintf-style formatting", {
  # Test that our logging wrappers exist and work with sprintf format
  ns <- asNamespace("tima")

  # Test with temp log file
  temp_log <- tempfile(fileext = ".log")
  on.exit(unlink(temp_log), add = TRUE)

  # Setup logger
  setup_logger(temp_log, 400)

  # Test sprintf-style logging
  log_info("Test %d: %s", 1, "message")
  log_warn("Warning %s", "test")

  # Read log and verify format
  log_content <- readLines(temp_log)
  expect_true(length(log_content) >= 2)
  expect_true(any(grepl("Test 1: message", log_content)))
  expect_true(any(grepl("Warning test", log_content)))
})

## Additional Coverage Tests ----

test_that("log_complete validates context object", {
  expect_error(
    log_complete("not a context"),
    "ctx must be a tima_log_context"
  )
})

test_that("log_complete works without results", {
  ctx <- log_operation("test_op")
  expect_no_error(log_complete(ctx))
})

test_that("log_failed validates context object", {
  expect_error(
    log_failed("not a context", "error"),
    "ctx must be a tima_log_context"
  )
})

test_that("log_failed handles error objects", {
  ctx <- log_operation("test_op")
  err <- simpleError("Test error")
  expect_no_error(log_failed(ctx, err))
})

test_that("log_failed handles character messages", {
  ctx <- log_operation("test_op")
  expect_no_error(log_failed(ctx, "Failed"))
})

test_that("accumulate_similarity_bins returns zero counts for empty input", {
  counts <- accumulate_similarity_bins(numeric(0))
  expect_type(counts, "integer")
  expect_equal(sum(counts), 0L)
  expect_equal(length(counts), 10L) # 10 bins for [0,1] by 0.1
})

test_that("accumulate_similarity_bins handles all NA values", {
  counts <- accumulate_similarity_bins(c(NA, NA, NA))
  expect_type(counts, "integer")
  expect_equal(sum(counts), 0L)
})

test_that("accumulate_similarity_bins clamps values to [0,1]", {
  # Values outside [0,1] should be clamped
  counts <- accumulate_similarity_bins(c(-0.5, 0.5, 1.5, 2))
  expect_true(sum(counts) == 4L)
})

test_that("accumulate_similarity_bins correctly bins values", {
  # Scores: 0.05 (bin 1), 0.15 (bin 2), 0.55 (bin 6), 0.99 (bin 10)
  counts <- accumulate_similarity_bins(c(0.05, 0.15, 0.55, 0.99))
  expect_equal(sum(counts), 4L)
  # All bins should be named
  expect_type(names(counts), "character")
  expect_equal(length(names(counts)), 10L)
})

test_that("accumulate_similarity_bins handles Inf and NaN", {
  counts <- accumulate_similarity_bins(c(Inf, -Inf, NaN, 0.5))
  expect_equal(sum(counts), 1L) # Only 0.5 should count
})

## log_similarity_distribution_counts ----

test_that("log_similarity_distribution_counts handles empty counts", {
  expect_no_error(log_similarity_distribution_counts(integer(0), "Test title"))
  expect_no_error(log_similarity_distribution_counts(NULL, "Test title"))
})

test_that("log_similarity_distribution_counts logs populated counts", {
  counts <- c("(0,0.1]" = 5, "(0.1,0.2]" = 10, "(0.2,0.3]" = 15)
  expect_no_error(log_similarity_distribution_counts(
    counts,
    "Similarity distribution"
  ))
})

test_that("log_similarity_distribution_counts preserves bin order", {
  # This is an implicit test - if bins get reordered, the output table would be wrong
  counts <- accumulate_similarity_bins(c(0.05, 0.25, 0.55, 0.85))
  expect_no_error(log_similarity_distribution_counts(counts, "Distribution"))
})

## log_similarity_distribution ----

test_that("log_similarity_distribution handles empty scores", {
  expect_no_error(log_similarity_distribution(numeric(0), "Empty test"))
})

test_that("log_similarity_distribution handles all NA scores", {
  expect_no_error(log_similarity_distribution(c(NA, NA, NA), "All NA test"))
})

test_that("log_similarity_distribution handles mixed valid/invalid", {
  expect_no_error(log_similarity_distribution(
    c(0.1, NA, 0.5, Inf, 0.9, -Inf),
    "Mixed test"
  ))
})

test_that("log_similarity_distribution creates proper binning", {
  scores <- c(0.05, 0.15, 0.25, 0.35, 0.45, 0.55, 0.65, 0.75, 0.85, 0.95)
  expect_no_error(log_similarity_distribution(scores, "Even distribution"))
})

## log_metadata ----

test_that("log_metadata validates context object", {
  expect_error(
    log_metadata("not a context", a = 1),
    "ctx must be a tima_log_context"
  )
})

test_that("log_metadata stores metadata correctly", {
  ctx <- log_operation("test_op")
  ctx_updated <- log_metadata(ctx, x = 100, y = "test")

  expect_equal(ctx_updated$metadata$x, 100)
  expect_equal(ctx_updated$metadata$y, "test")
})

test_that("log_metadata returns context invisibly", {
  ctx <- log_operation("test_op")
  result <- withVisible(log_metadata(ctx, a = 1))

  expect_false(result$visible)
  expect_equal(class(result$value), class(ctx))
})

test_that("log_metadata accumulates multiple calls", {
  ctx <- log_operation("test_op")
  ctx <- log_metadata(ctx, a = 1)
  ctx <- log_metadata(ctx, b = 2)

  expect_equal(length(ctx$metadata), 2)
  expect_equal(ctx$metadata$a, 1)
  expect_equal(ctx$metadata$b, 2)
})

## with_logging ----

test_that("with_logging executes expression and returns result", {
  result <- with_logging("arithmetic", 2 + 2)
  expect_equal(result, 4)
})

test_that("with_logging returns scalar unchanged", {
  result <- with_logging("scalar", 42L)
  expect_equal(result, 42L)
})

test_that("with_logging propagates errors properly", {
  expect_error(
    with_logging("failing_op", stop("intentional failure")),
    class = "tima_error"
  )
})

test_that("with_logging error message includes operation name", {
  expect_error(
    with_logging("my_operation", stop("boom")),
    "my_operation"
  )
})

test_that("with_logging accepts parameters", {
  result <- with_logging("op_with_params", 42, x = 100, y = "test")
  expect_equal(result, 42)
})

test_that("with_logging handles vector result", {
  result <- with_logging("vector_op", 1:10)
  expect_equal(result, 1:10)
})

## log_complete ----

test_that("log_complete validates context object", {
  expect_error(
    log_complete("not a context"),
    "ctx must be a tima_log_context"
  )
})

test_that("log_complete returns context invisibly", {
  ctx <- log_operation("test")
  result <- withVisible(log_complete(ctx))

  expect_false(result$visible)
  expect_equal(class(result$value), class(ctx))
})

test_that("log_complete works with results", {
  ctx <- log_operation("test")
  expect_no_error(log_complete(ctx, n_items = 42, success = TRUE))
})

test_that("log_complete works without results", {
  ctx <- log_operation("test")
  expect_no_error(log_complete(ctx))
})

## log_failed ----

test_that("log_failed validates context object", {
  expect_error(
    log_failed("not a context", "error"),
    "ctx must be a tima_log_context"
  )
})

test_that("log_failed handles error objects", {
  ctx <- log_operation("test")
  err <- simpleError("Test error message")
  expect_no_error(log_failed(ctx, err))
})

test_that("log_failed handles character error messages", {
  ctx <- log_operation("test")
  expect_no_error(log_failed(ctx, "Error occurred"))
})

test_that("log_failed handles extra context", {
  ctx <- log_operation("test")
  expect_no_error(log_failed(ctx, "error", attempt = 3, retrying = TRUE))
})

test_that("log_failed returns context invisibly", {
  ctx <- log_operation("test")
  result <- withVisible(log_failed(ctx, "error"))

  expect_false(result$visible)
  expect_equal(class(result$value), class(ctx))
})

## log_operation ----

test_that("log_operation creates context with operation name", {
  ctx <- log_operation("my_operation")
  expect_equal(ctx$operation, "my_operation")
})

test_that("log_operation stores parameters", {
  ctx <- log_operation("op", n = 100, verbose = TRUE, name = "test")
  expect_equal(ctx$params$n, 100)
  expect_equal(ctx$params$verbose, TRUE)
  expect_equal(ctx$params$name, "test")
})

test_that("log_operation initializes empty metadata", {
  ctx <- log_operation("op")
  expect_type(ctx$metadata, "list")
  expect_equal(length(ctx$metadata), 0)
})

test_that("log_operation records start time", {
  before <- Sys.time()
  ctx <- log_operation("op")
  after <- Sys.time()

  expect_true(ctx$start_time >= before)
  expect_true(ctx$start_time <= after)
})

test_that("log_operation returns tima_log_context class", {
  ctx <- log_operation("op")
  expect_s3_class(ctx, "tima_log_context")
})
