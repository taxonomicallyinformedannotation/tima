# Test Suite: go_to_cache ----

library(testthat)

# Input Validation Tests ----

test_that("go_to_cache validates dir parameter", {
  expect_error(
    go_to_cache(dir = NULL),
    "Provide a non-NULL character string."
  )

  expect_error(
    go_to_cache(dir = 123),
    "Ensure the parameter is a length-1 character value."
  )

  expect_error(
    go_to_cache(dir = c(".cache1", ".cache2")),
    "Ensure the parameter is a length-1 character value."
  )

  expect_error(
    go_to_cache(dir = ""),
    "Provide a non-empty string."
  )

  expect_error(
    go_to_cache(dir = character(0)),
    "Ensure the parameter is a length-1 character value."
  )
})
