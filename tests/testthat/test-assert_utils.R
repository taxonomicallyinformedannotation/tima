# Test Suite: assert_utils ----
# Tests for assertion helper functions in assert_utils.R

library(testthat)

# ── assert_flag ──────────────────────────────────────────────────────────────

test_that("assert_flag returns invisible TRUE for valid flags", {
  expect_invisible(assert_flag(TRUE))
  expect_invisible(assert_flag(FALSE))
  expect_true(assert_flag(TRUE))
  expect_true(assert_flag(FALSE))
})

test_that("assert_flag errors on non-logical input", {
  expect_error(assert_flag(1L), class = "tima_error")
  expect_error(assert_flag("TRUE"), class = "tima_error")
  expect_error(assert_flag(1.0), class = "tima_error")
  expect_error(assert_flag(NULL), class = "tima_error")
})

test_that("assert_flag errors on NA logical", {
  expect_error(assert_flag(NA), class = "tima_error")
  expect_error(assert_flag(NA_integer_), class = "tima_error")
})

test_that("assert_flag errors on logical vector length > 1", {
  expect_error(assert_flag(c(TRUE, FALSE)), class = "tima_error")
  expect_error(assert_flag(c(TRUE, TRUE, TRUE)), class = "tima_error")
})

test_that("assert_flag uses arg_name in error message", {
  my_flag <- 42L
  err <- tryCatch(assert_flag(my_flag, "my_flag"), error = function(e) e)
  expect_match(conditionMessage(err), "my_flag", fixed = TRUE)
})

# ── assert_positive_integer ──────────────────────────────────────────────────

test_that("assert_positive_integer returns invisible TRUE for valid values", {
  expect_invisible(assert_positive_integer(1L))
  expect_invisible(assert_positive_integer(5L))
  expect_invisible(assert_positive_integer(100))
  expect_true(assert_positive_integer(1L))
})

test_that("assert_positive_integer accepts zero when allow_zero = TRUE", {
  expect_invisible(assert_positive_integer(0L, allow_zero = TRUE))
  expect_invisible(assert_positive_integer(0, allow_zero = TRUE))
})

test_that("assert_positive_integer errors on zero by default", {
  expect_error(assert_positive_integer(0L), class = "tima_error")
  expect_error(assert_positive_integer(0), class = "tima_error")
})

test_that("assert_positive_integer errors on negative values", {
  expect_error(assert_positive_integer(-1L), class = "tima_error")
  expect_error(assert_positive_integer(-100), class = "tima_error")
  expect_error(
    assert_positive_integer(-1L, allow_zero = TRUE),
    ">= 0",
    class = "tima_error"
  )
})

test_that("assert_positive_integer errors on non-integer values", {
  expect_error(assert_positive_integer(1.5), class = "tima_error")
  expect_error(assert_positive_integer(2.7), class = "tima_error")
})

test_that("assert_positive_integer errors on non-numeric input", {
  expect_error(assert_positive_integer("1"), class = "tima_error")
  expect_error(assert_positive_integer(TRUE), class = "tima_error")
  expect_error(assert_positive_integer(NULL), class = "tima_error")
})

test_that("assert_positive_integer errors on NA", {
  expect_error(assert_positive_integer(NA_integer_), class = "tima_error")
  expect_error(assert_positive_integer(NA_real_), class = "tima_error")
})

test_that("assert_positive_integer errors on vector length > 1", {
  expect_error(assert_positive_integer(c(1L, 2L)), class = "tima_error")
})

# ── assert_scalar_numeric ────────────────────────────────────────────────────

test_that("assert_scalar_numeric returns invisible TRUE for valid values", {
  expect_invisible(assert_scalar_numeric(1.0, "x"))
  expect_invisible(assert_scalar_numeric(0, "x"))
  expect_invisible(assert_scalar_numeric(-5.5, "x"))
  expect_true(assert_scalar_numeric(42, "x"))
})

test_that("assert_scalar_numeric respects min/max bounds", {
  expect_invisible(assert_scalar_numeric(5, "x", min = 0, max = 10))
  expect_error(
    assert_scalar_numeric(-1, "x", min = 0, max = 10),
    class = "tima_error"
  )
  expect_error(
    assert_scalar_numeric(11, "x", min = 0, max = 10),
    class = "tima_error"
  )
})

test_that("assert_scalar_numeric errors on non-numeric input", {
  expect_error(assert_scalar_numeric("1.0", "x"), class = "tima_error")
  expect_error(assert_scalar_numeric(TRUE, "x"), class = "tima_error")
  expect_error(assert_scalar_numeric(NULL, "x"), class = "tima_error")
})

test_that("assert_scalar_numeric errors on vector length > 1", {
  expect_error(assert_scalar_numeric(c(1.0, 2.0), "x"), class = "tima_error")
})

test_that("assert_scalar_numeric errors on NA by default", {
  expect_error(assert_scalar_numeric(NA_real_, "x"), class = "tima_error")
})

test_that("assert_scalar_numeric allows NA when allow_na = TRUE", {
  expect_invisible(assert_scalar_numeric(NA_real_, "x", allow_na = TRUE))
})

test_that("assert_scalar_numeric includes param_name in error message", {
  err <- tryCatch(
    assert_scalar_numeric("bad", "my_param"),
    error = function(e) e
  )
  expect_match(conditionMessage(err), "my_param", fixed = TRUE)
})

# ── assert_choice (internal) ─────────────────────────────────────────────────

test_that("assert_choice returns invisible TRUE for valid choices", {
  expect_invisible(assert_choice("pos", c("pos", "neg"), "ms_mode"))
  expect_invisible(assert_choice("neg", c("pos", "neg"), "ms_mode"))
})

test_that("assert_choice errors on invalid choice", {
  expect_error(
    assert_choice("both", c("pos", "neg"), "ms_mode"),
    class = "tima_error"
  )
})

test_that("assert_choice errors on non-character input", {
  expect_error(
    assert_choice(1, c("pos", "neg"), "ms_mode"),
    class = "tima_error"
  )
  expect_error(
    assert_choice(c("pos", "neg"), c("pos", "neg"), "ms_mode"),
    class = "tima_error"
  )
})
