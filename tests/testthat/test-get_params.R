# Test Suite: get_params ----

library(testthat)

test_that("get_params validates required step argument", {
  expect_error(
    get_params(step = NULL),
    "step name must be provided and non-empty",
    class = "tima_validation_error"
  )

  expect_error(
    get_params(step = NA_character_),
    "step name must be provided and non-empty",
    class = "tima_validation_error"
  )

  expect_error(
    get_params(step = 123),
    "step must be a single character string",
    class = "tima_validation_error"
  )
})

test_that("get_params errors for unknown step", {
  expect_error(
    get_params(step = "does_not_exist"),
    "step does not exist",
    class = "tima_validation_error"
  )
})

test_that("get_params loads default parameters for prepare steps", {
  params_default <- get_params(step = "prepare_params")
  expect_type(params_default, "list")
  expect_true(length(params_default) > 0L)

  params_advanced <- get_params(step = "prepare_params_advanced")
  expect_type(params_advanced, "list")
  expect_true(length(params_advanced) > 0L)
})
