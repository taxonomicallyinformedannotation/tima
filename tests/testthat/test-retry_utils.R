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
