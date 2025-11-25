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

## format_percent ----

test_that("format_percent formats percentages correctly", {
  expect_equal(format_percent(0.5), "50%")
  expect_equal(format_percent(0.755), "75.5%")
  expect_equal(format_percent(0.7555), "75.6%")
  expect_equal(format_percent(1), "100%")
  expect_equal(format_percent(0), "0%")
})

test_that("format_percent handles different digit arguments", {
  expect_equal(format_percent(0.12345, digits = 0), "12%")
  expect_equal(format_percent(0.12345, digits = 1), "12.3%")
  expect_equal(format_percent(0.12345, digits = 2), "12.35%")
})

## log_with_count ----

test_that("log_with_count logs correctly with count", {
  # Just check it doesn't error
  expect_silent(log_with_count("Test message", n = 1234))
  expect_silent(log_with_count("Test message", n = 1000000))
})

test_that("log_with_count logs correctly with count and elapsed", {
  expect_silent(log_with_count("Test message", n = 1234, elapsed = 5.2))
})

test_that("log_with_count logs correctly with just elapsed", {
  expect_silent(log_with_count("Test message", elapsed = 2.5))
})

test_that("log_with_count logs correctly with neither", {
  expect_silent(log_with_count("Test message"))
})

## log_file_op ----

test_that("log_file_op logs file operations correctly", {
  # Create temp file for testing
  tmp <- withr::local_tempfile()
  writeLines("test", tmp)
  file_size <- file.info(tmp)$size

  expect_silent(log_file_op(
    "Exported",
    tmp,
    size_bytes = file_size,
    n_rows = 100
  ))
  expect_silent(log_file_op("Downloaded", tmp, size_bytes = file_size))
  expect_silent(log_file_op("Loaded", tmp))
})

test_that("log_file_op handles missing size and rows", {
  tmp <- withr::local_tempfile()
  writeLines("test", tmp)

  expect_silent(log_file_op("Action", tmp, n_rows = 100))
  expect_silent(log_file_op("Action", tmp, size_bytes = 1024))
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
  expect_silent(log_with_count("Test", n = 0))
  expect_silent(log_file_op("Test", "nonexistent.txt"))
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
