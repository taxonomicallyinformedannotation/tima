# Test Suite: errors_utils ----
# Tests format_error and .fuzzy_match.

library(testthat)

test_that("format_error returns a non-empty string containing the problem", {
  msg <- format_error(problem = "Invalid mode", fix = "Use 'pos' or 'neg'")
  expect_type(msg, "character")
  expect_true(nzchar(msg))
  expect_true(grepl("Invalid mode", msg))
})

test_that("format_error includes expected, received, location when provided", {
  msg <- format_error(
    problem = "Wrong type",
    expected = "character",
    received = "numeric",
    location = "my_param"
  )
  expect_true(grepl("character", msg))
  expect_true(grepl("numeric", msg))
  expect_true(grepl("my_param", msg))
})

test_that("format_error works with only the problem argument", {
  expect_no_error(msg <- format_error(problem = "Oops"))
  expect_true(grepl("Oops", msg))
})

test_that(".fuzzy_match returns closest match within distance", {
  fuzz <- getFromNamespace(".fuzzy_match", "tima")
  expect_equal(fuzz("positiv", c("pos", "neg", "positive")), "positive")
})

test_that(".fuzzy_match returns NULL when no match is close enough", {
  fuzz <- getFromNamespace(".fuzzy_match", "tima")
  result <- fuzz(
    value = "xyzzy",
    valid_values = c("pos", "neg"),
    max_distance = 2L
  )
  expect_null(result)
})

test_that(".fuzzy_match handles non-character inputs gracefully", {
  fuzz <- getFromNamespace(".fuzzy_match", "tima")
  expect_null(fuzz(123, c("pos", "neg")))
  expect_null(fuzz("pos", c(1L, 2L)))
})
