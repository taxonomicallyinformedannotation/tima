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
