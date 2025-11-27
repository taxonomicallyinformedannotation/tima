# Test Suite: weights_utils ----

library(testthat)

test_that("compute_weighted_sum normalizes weights", {
  v1 <- c(0.6, 0.8)
  v2 <- c(0.4, 0.2)
  w <- c(2, 2)
  result <- compute_weighted_sum(v1, v2, weights = w)
  expected <- (v1 * 0.5 + v2 * 0.5) # weights normalized to [0.5, 0.5]
  expect_equal(result, expected)

  w2 <- c(1, 3)
  result2 <- compute_weighted_sum(v1, v2, weights = w2)
  expected2 <- (v1 * 0.25 + v2 * 0.75) # weights normalized to [0.25, 0.75]
  expect_equal(result2, expected2)
})

test_that("compute_weighted_sum handles three components", {
  v1 <- c(0.5, 0.6)
  v2 <- c(0.3, 0.4)
  v3 <- c(0.2, 0.1)
  w <- c(1, 1, 1)
  result <- compute_weighted_sum(v1, v2, v3, weights = w)
  expected <- (v1 + v2 + v3) / 3 # equal weights
  expect_equal(result, expected)
})

test_that("compute_weighted_sum matches original formula", {
  # Simulate original formula from weight_bio:
  # (1/(w_bio + w_spec)) * w_bio * score_bio + (1/(w_bio + w_spec)) * w_spec * score_spec
  score_bio <- c(0.7, 0.8, 0.9)
  score_spec <- c(0.5, 0.6, 0.4)
  w_bio <- 0.4
  w_spec <- 0.6

  result <- compute_weighted_sum(
    score_bio,
    score_spec,
    weights = c(w_bio, w_spec)
  )

  # Original formula
  expected <- (w_bio * score_bio + w_spec * score_spec) / (w_bio + w_spec)

  expect_equal(result, expected)
})
