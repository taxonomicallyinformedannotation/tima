# Test Suite: setup_logger ----

library(testthat)

test_that("setup_logger validates filename parameter", {
  expect_error(
    setup_logger(filename = NULL),
    "Log filename must be a non-empty character string"
  )

  expect_error(
    setup_logger(filename = ""),
    "Log filename must be a non-empty character string"
  )

  expect_error(
    setup_logger(filename = 123),
    "Log filename must be a non-empty character string"
  )

  expect_error(
    setup_logger(filename = c("file1.log", "file2.log")),
    "Log filename must be a non-empty character string"
  )
})

test_that("setup_logger creates log file", {
  log_file <- tempfile(fileext = ".log")

  expect_silent(setup_logger(filename = log_file))

  # Write a test message
  logger::log_info("Test message")

  # File should be created
  expect_true(file.exists(log_file))
})

test_that("setup_logger sets threshold correctly", {
  log_file <- tempfile(fileext = ".log")

  # Test different thresholds
  expect_silent(setup_logger(filename = log_file, threshold = logger::INFO))
  expect_silent(setup_logger(filename = log_file, threshold = logger::WARN))
  expect_silent(setup_logger(filename = log_file, threshold = logger::ERROR))
  expect_silent(setup_logger(filename = log_file, threshold = logger::DEBUG))
  expect_silent(setup_logger(filename = log_file, threshold = logger::TRACE))
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

  setup_logger(filename = log_file, threshold = logger::INFO)
  logger::log_info("Test info message")
  logger::log_warn("Test warning message")

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
  setup_logger(filename = log_file, threshold = logger::WARN)

  logger::log_info("This should not appear")
  logger::log_warn("This should appear")
  logger::log_error("This should also appear")

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
  logger::log_info("Test from env")

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
  logger::log_info("Should not appear")
  logger::log_warn("Should appear")

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

  for (level in levels) {
    withr::local_envvar(
      TIMA_LOG_FILE = log_file,
      TIMA_LOG_LEVEL = level
    )

    expect_silent(init_logging())
  }
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
