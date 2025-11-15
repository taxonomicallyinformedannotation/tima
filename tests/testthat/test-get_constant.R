# ==============================================================================
# Test Suite: get_constant / validate_against_constant
# ==============================================================================

library(testthat)
library(tima)

test_that("get_constant returns existing constant", {
  val <- tima:::get_constant("DEFAULT_HC_SCORE_BIO_MIN")
  expect_equal(val, 0.85)
})

test_that("get_constant falls back to default when missing", {
  expect_warning(
    missing_val <- tima:::get_constant("NON_EXISTING_CONSTANT", default = 123),
    NA
  )
  expect_equal(missing_val, 123)
})

test_that("get_constant errors if missing and no default", {
  expect_error(tima:::get_constant("NON_EXISTING_CONSTANT"), "not found")
})

test_that("validate_against_constant succeeds for valid ms mode", {
  expect_true(tima:::validate_against_constant("pos", "VALID_MS_MODES"))
})

test_that("validate_against_constant errors for invalid ms mode", {
  expect_error(
    tima:::validate_against_constant("invalid", "VALID_MS_MODES"),
    "Invalid value"
  )
})
