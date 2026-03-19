# Test Suite: retry_utils ----
# Tests with_retry exponential-backoff logic without real network calls.

library(testthat)

test_that("with_retry returns result immediately on success", {
  calls <- 0L
  result <- with_retry(
    {
      calls <- calls + 1L
      42L
    },
    max_attempts = 3L,
    backoff = 0,
    silent = TRUE
  )
  expect_equal(result, 42L)
  expect_equal(calls, 1L)
})

test_that("with_retry retries on error and eventually succeeds", {
  calls <- 0L
  result <- with_retry(
    {
      calls <- calls + 1L
      if (calls < 3L) {
        stop("transient error")
      }
      "success"
    },
    max_attempts = 5L,
    backoff = 0,
    silent = TRUE
  )
  expect_equal(result, "success")
  expect_equal(calls, 3L)
})

test_that("with_retry throws after max_attempts exhausted", {
  calls <- 0L
  expect_error(
    with_retry(
      {
        calls <- calls + 1L
        stop("always fails")
      },
      max_attempts = 3L,
      backoff = 0,
      silent = TRUE
    ),
    "always fails"
  )
  expect_equal(calls, 3L)
})

test_that("with_retry validates max_attempts must be positive integer", {
  expect_error(with_retry(TRUE, max_attempts = 0L), "max_attempts must be > 0")
  expect_error(with_retry(TRUE, max_attempts = -1L), "max_attempts must be > 0")
})

test_that("with_retry validates backoff must be non-negative", {
  expect_error(with_retry(TRUE, max_attempts = 1L, backoff = -1), "backoff")
})

test_that("with_retry logs retry warning when not silent", {
  calls <- 0L
  expect_no_error(
    with_retry(
      {
        calls <- calls + 1L
        if (calls < 2L) {
          stop("transient")
        }
        "ok"
      },
      max_attempts = 3L,
      backoff = 0,
      silent = FALSE
    )
  )
})

test_that("with_retry returns complex objects unchanged", {
  df <- data.frame(a = 1:3, b = letters[1:3])
  result <- with_retry(df, max_attempts = 1L, backoff = 0, silent = TRUE)
  expect_equal(result, df)
})

test_that("with_retry error class is tima_runtime_error", {
  expect_error(
    with_retry(stop("boom"), max_attempts = 1L, backoff = 0, silent = TRUE),
    class = "tima_runtime_error"
  )
})

## is_retryable_error ----

test_that("is_retryable_error returns TRUE for timeout errors", {
  expect_true(is_retryable_error(simpleError("connection timeout")))
  expect_true(is_retryable_error(simpleError("request timed out")))
})

test_that("is_retryable_error returns TRUE for connection errors", {
  expect_true(is_retryable_error(simpleError("connection failed")))
  expect_true(is_retryable_error(simpleError("Connection refused")))
  expect_true(is_retryable_error(simpleError("connection reset by peer")))
})

test_that("is_retryable_error returns TRUE for network errors", {
  expect_true(is_retryable_error(simpleError("network unreachable")))
  expect_true(is_retryable_error(simpleError(
    "temporary failure in name resolution"
  )))
})

test_that("is_retryable_error returns TRUE for HTTP 5xx/429 status codes", {
  expect_true(is_retryable_error(simpleError("HTTP 503 Service Unavailable")))
  expect_true(is_retryable_error(simpleError("502 Bad Gateway")))
  expect_true(is_retryable_error(simpleError("504 Gateway Timeout")))
  expect_true(is_retryable_error(simpleError("429 Too Many Requests")))
})

test_that("is_retryable_error returns FALSE for permanent errors", {
  expect_false(is_retryable_error(simpleError("object not found")))
  expect_false(is_retryable_error(simpleError("argument is of wrong type")))
  expect_false(is_retryable_error(simpleError("404 Not Found")))
  expect_false(is_retryable_error(simpleError("401 Unauthorized")))
  expect_false(is_retryable_error(simpleError("403 Forbidden")))
})

test_that("is_retryable_error is case-insensitive", {
  expect_true(is_retryable_error(simpleError("TIMEOUT")))
  expect_true(is_retryable_error(simpleError("Connection RESET")))
})

## with_http_retry ----

test_that("with_http_retry succeeds on first attempt", {
  result <- with_http_retry("response_value", max_attempts = 3L, backoff = 0)
  expect_equal(result, "response_value")
})

test_that("with_http_retry retries and returns result on success", {
  calls <- 0L
  # Suppress warning about interrupted promise evaluation (expected during Sys.sleep)
  result <- suppressWarnings(with_http_retry(
    {
      calls <- calls + 1L
      if (calls < 2L) {
        stop("connection failed")
      }
      "success"
    },
    max_attempts = 3L,
    backoff = 0
  ))
  expect_equal(result, "success")
  expect_equal(calls, 2L)
})

test_that("with_http_retry throws tima_runtime_error after exhausting retries", {
  # Suppress warning about interrupted promise evaluation (expected during Sys.sleep)
  expect_error(
    suppressWarnings(with_http_retry(
      stop("always fails"),
      max_attempts = 2L,
      backoff = 0
    )),
    class = "tima_runtime_error"
  )
})

## with_smart_retry ----

test_that("with_smart_retry succeeds immediately with no error", {
  result <- with_smart_retry(
    42L,
    max_attempts = 3L,
    backoff = 0
  )
  expect_equal(result, 42L)
})

test_that("with_smart_retry retries on transient (retryable) errors", {
  calls <- 0L
  result <- with_smart_retry(
    {
      calls <- calls + 1L
      if (calls < 3L) {
        stop("connection failed")
      }
      "done"
    },
    max_attempts = 5L,
    backoff = 0
  )
  expect_equal(result, "done")
  expect_equal(calls, 3L)
})

test_that("with_smart_retry does NOT retry on permanent errors", {
  calls <- 0L
  expect_error(
    with_smart_retry(
      {
        calls <- calls + 1L
        stop("object not found")
      },
      max_attempts = 5L,
      backoff = 0
    ),
    class = "tima_error"
  )
  # Should have only been called once - permanent error, no retry
  expect_equal(calls, 1L)
})

test_that("with_smart_retry throws tima_runtime_error after exhausting transient retries", {
  expect_error(
    with_smart_retry(
      stop("connection timeout"),
      max_attempts = 2L,
      backoff = 0
    ),
    class = "tima_runtime_error"
  )
})

test_that("with_smart_retry uses custom operation_name in error message", {
  expect_error(
    with_smart_retry(
      stop("connection failed"),
      max_attempts = 1L,
      backoff = 0,
      operation_name = "my_special_op"
    ),
    "my_special_op"
  )
})
