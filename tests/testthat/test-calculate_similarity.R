# Test Suite: calculate_similarity ----

library(testthat)

## Internal Utility Helpers ----

make_spec <- function(mz, int) {
  matrix(c(mz, int), ncol = 2, dimnames = list(NULL, c("mz", "int")))
}

TEST_QUERY <- make_spec(c(100, 150, 200, 250), c(10, 200, 500, 50))
TEST_TARGET <- make_spec(c(100, 151, 199, 300), c(12, 180, 520, 40))

## Basic method tests ----

test_that("calculate_similarity works with entropy method", {
  score <- calculate_similarity(
    method = "entropy",
    query_spectrum = TEST_QUERY,
    target_spectrum = TEST_TARGET,
    query_precursor = 250,
    target_precursor = 300,
    dalton = 0.01,
    ppm = 10
  )
  expect_type(score, "double")
  expect_true(score >= 0)
})

test_that("calculate_similarity works with gnps method returning matches", {
  res <- calculate_similarity(
    method = "gnps",
    query_spectrum = TEST_QUERY,
    target_spectrum = TEST_TARGET,
    query_precursor = 250,
    target_precursor = 300,
    dalton = 0.01,
    ppm = 10,
    return_matched_peaks = TRUE
  )
  expect_type(res$score, "double")
  expect_true(res$matches >= 0)
})

test_that("calculate_similarity works with cosine method", {
  score <- calculate_similarity(
    method = "cosine",
    query_spectrum = TEST_QUERY,
    target_spectrum = TEST_TARGET,
    query_precursor = 250,
    target_precursor = 300,
    dalton = 0.01,
    ppm = 10
  )
  expect_type(score, "double")
})

## Validation tests ----

test_that("calculate_similarity validates method", {
  expect_error(
    calculate_similarity(
      method = "foo",
      query_spectrum = TEST_QUERY,
      target_spectrum = TEST_TARGET,
      query_precursor = 1,
      target_precursor = 2,
      dalton = 0.01,
      ppm = 10
    ),
    "Invalid method"
  )
})

test_that("calculate_similarity returns 0 for empty spectra", {
  empty <- matrix(numeric(), ncol = 2)
  res <- calculate_similarity(
    method = "cosine",
    query_spectrum = empty,
    target_spectrum = TEST_TARGET,
    query_precursor = 1,
    target_precursor = 2,
    dalton = 0.01,
    ppm = 10,
    return_matched_peaks = TRUE
  )
  expect_equal(res$score, 0.0)
  expect_equal(res$matches, 0L)
})

# test_that("calculate_similarity rejects non-matrix input", {
#   expect_error(
#     calculate_similarity(
#       method = "cosine",
#       query_spectrum = as.list(TEST_QUERY),
#       target_spectrum = TEST_TARGET,
#       query_precursor = 1,
#       target_precursor = 2,
#       dalton = 0.01,
#       ppm = 10
#     ),
#     "Spectra must be matrices"
#   )
# })

test_that("calculate_similarity rejects negative tolerances", {
  expect_error(
    calculate_similarity(
      method = "cosine",
      query_spectrum = TEST_QUERY,
      target_spectrum = TEST_TARGET,
      query_precursor = 1,
      target_precursor = 2,
      dalton = -0.01,
      ppm = 10
    ),
    "non-negative"
  )
})
