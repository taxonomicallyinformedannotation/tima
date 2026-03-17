# Test Suite: transform_score_sirius_csi ----

library(testthat)

test_that("transform_score_sirius_csi returns NA for NULL", {
  expect_true(is.na(transform_score_sirius_csi(NULL)))
})

test_that("transform_score_sirius_csi transforms numeric vector", {
  scores <- c(-50, 0, 50)
  transformed <- transform_score_sirius_csi(scores)
  expect_type(transformed, "double")
  expect_equal(length(transformed), length(scores))
  expect_true(all(transformed > 0 & transformed < 1, na.rm = TRUE))
})

test_that("transform_score_sirius_csi validates scale", {
  expect_error(transform_score_sirius_csi(1, scale = -1), "scale")
})

test_that("transform_score_sirius_csi is monotone increasing", {
  scores <- c(-100, -50, -10, 0, 10, 50, 100)
  out <- transform_score_sirius_csi(scores)
  expect_true(all(diff(out) > 0))
})

test_that("transform_score_sirius_csi midpoint at -K gives ~0.5", {
  # sigmoid((-K + K)/scale) = sigmoid(0) = 0.5
  out <- transform_score_sirius_csi(-50, K = 50, scale = 10)
  expect_equal(out, 0.5, tolerance = 1e-9)
})

test_that("transform_score_sirius_csi preserves NA values", {
  scores <- c(-20, NA_real_, 10)
  out <- transform_score_sirius_csi(scores)
  expect_true(is.na(out[2]))
  expect_false(is.na(out[1]))
  expect_false(is.na(out[3]))
})

test_that("transform_score_sirius_csi errors on non-numeric input", {
  expect_error(
    transform_score_sirius_csi("bad"),
    "numeric",
    class = "tima_validation_error"
  )
})

test_that("transform_score_sirius_csi respects custom K and scale", {
  # With K=0, scale=1: midpoint at score=0
  out <- transform_score_sirius_csi(0, K = 0, scale = 1)
  expect_equal(out, 0.5, tolerance = 1e-9)
})
