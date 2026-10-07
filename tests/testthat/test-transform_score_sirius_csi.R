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

# New-default (K=100, scale=20) coverage ----

test_that("transform_score_sirius_csi new defaults: midpoint at CSI=-100", {
  # sigmoid((-100 + 100) / 20) = sigmoid(0) = 0.5
  out <- transform_score_sirius_csi(-100)
  expect_equal(out, 0.5, tolerance = 1e-9)
})

test_that("transform_score_sirius_csi new defaults: excellent hit near 1", {
  # CSI = -10 (awesome) -> sigmoid(90/20) ~ 0.989
  out <- transform_score_sirius_csi(-10)
  expect_gt(out, 0.98)
  expect_lt(out, 1)
})

test_that("transform_score_sirius_csi new defaults: poor hit near 0", {
  # CSI = -200 -> sigmoid(-100/20) ~ 0.0067
  out <- transform_score_sirius_csi(-200)
  expect_lt(out, 0.01)
  expect_gt(out, 0)
})

test_that("transform_score_sirius_csi new defaults: maps realistic range to [0,1]", {
  # Realistic SIRIUS range for CSI:FingerID: values up to 0 (best near 0)
  scores <- seq(-400, 0, by = 50)
  out <- transform_score_sirius_csi(scores)
  expect_true(all(out > 0 & out < 1))
  expect_true(all(diff(out) > 0)) # monotone
})

test_that("transform_score_sirius_csi old defaults give same math as before", {
  # Verify backward-compat with explicit old defaults
  out_old_100 <- transform_score_sirius_csi(-100, K = 50, scale = 10)
  expect_equal(out_old_100, 1 / (1 + exp(5)), tolerance = 1e-9) # sigmoid(-5)
})

test_that("transform_score_sirius_csi default midpoint sits at -100", {
  # The documented K = 100 / scale = 20 defaults place the sigmoid midpoint at
  # score = -100, not at -50 as the previous defaults did.
  expect_equal(transform_score_sirius_csi(-100), 0.5, tolerance = 1e-9)
  # Score of 0 (best possible CSI) is not exactly 1.
  expect_equal(transform_score_sirius_csi(0), 1 / (1 + exp(-5)))
})

test_that("transform_score_sirius_csi matches the documented reference values", {
  out <- transform_score_sirius_csi(c(-200, -100, -10))

  expect_equal(out[1], 1 / (1 + exp(5)), tolerance = 1e-9)
  expect_equal(out[2], 0.5, tolerance = 1e-9)
  expect_equal(out[3], 1 / (1 + exp(-4.5)), tolerance = 1e-9)

  # The rule of thumb: > -10 excellent, > -100 acceptable, <= -100 weak.
  expect_gt(out[3], 0.98)
  expect_gt(out[2], 0.49)
  expect_lt(out[2], 0.51)
  expect_lt(out[1], 0.01)
})

test_that("transform_score_sirius_csi scale controls the steepness", {
  # A wider scale flattens the curve: the midpoint stays at 0.5 but a point
  # one scale away from it moves less.
  sharp <- transform_score_sirius_csi(-80, K = 100, scale = 20)
  flat <- transform_score_sirius_csi(-80, K = 100, scale = 40)

  expect_equal(sharp, 1 / (1 + exp(-1)), tolerance = 1e-9)
  expect_equal(flat, 1 / (1 + exp(-0.5)), tolerance = 1e-9)
  expect_lt(flat - 0.5, sharp - 0.5)

  # K shifts the midpoint: with K = 50 the midpoint is at -50, so -40 (and
  # -60) sit one and a half scales above and below it.
  expect_equal(transform_score_sirius_csi(-50, K = 50, scale = 20), 0.5, tolerance = 1e-9)
  expect_equal(
    transform_score_sirius_csi(-30, K = 50, scale = 20),
    1 / (1 + exp(-1)),
    tolerance = 1e-9
  )
  expect_equal(
    transform_score_sirius_csi(-70, K = 50, scale = 20),
    1 / (1 + exp(1)),
    tolerance = 1e-9
  )
})

test_that("transform_score_sirius_csi validates K and scale as scalars", {
  expect_error(
    transform_score_sirius_csi(-10, K = c(1, 2)),
    class = "tima_validation_error"
  )
  expect_error(
    transform_score_sirius_csi(-10, K = "100"),
    class = "tima_validation_error"
  )
  expect_error(
    transform_score_sirius_csi(-10, scale = c(1, 2)),
    class = "tima_validation_error"
  )
  expect_error(
    transform_score_sirius_csi(-10, scale = "20"),
    class = "tima_validation_error"
  )
  # scale must be strictly positive.
  expect_error(
    transform_score_sirius_csi(-10, scale = 0),
    class = "tima_validation_error"
  )
  expect_error(transform_score_sirius_csi(-10, scale = -1), "scale")
  # K may be negative.
  expect_no_error(transform_score_sirius_csi(-10, K = -100))
})
