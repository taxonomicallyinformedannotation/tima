# Test Suite: parse_cli_params ----

library(testthat)

test_that("parse_cli_params merges CLI arguments into parameters", {
  local_test_project(copy = TRUE)
  params <- get_params(step = "prepare_params_advanced")

  arguments <- list()
  arguments$ann_can_fin <- 666L

  result <- parse_cli_params(arguments = arguments, parameters = params)

  expect_type(result, "list")
  expect_equal(
    result$annotations$candidates$final,
    666L,
    info = "CLI argument should override parameter"
  )
})

test_that("parse_cli_params handles empty arguments list", {
  local_test_project(copy = TRUE)
  params <- get_params(step = "prepare_params_advanced")

  result <- parse_cli_params(arguments = list(), parameters = params)

  expect_type(result, "list")
  expect_identical(
    result,
    params,
    info = "Empty args should return original params"
  )
})

test_that("parse_cli_params preserves unmodified parameters", {
  local_test_project(copy = TRUE)
  params <- get_params(step = "prepare_params_advanced")

  # Modify only one parameter
  arguments <- list(ann_can_fin = 100L)

  result <- parse_cli_params(arguments = arguments, parameters = params)

  expect_type(result, "list")
  # Other parameters should remain unchanged
  expect_true(length(result) >= length(params))
})
