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

test_that("compute_weighted_sum handles NA in one component", {
  # MS1-only case: initial score is NA, but biological score is valid
  # NA is treated as 0 in the calculation
  v1 <- c(0.8, NA, 0.9) # biological score
  v2 <- c(0.5, 0.6, NA) # initial/spectral score
  w <- c(0.4, 0.6)

  result <- compute_weighted_sum(v1, v2, weights = w)

  # Row 1: both valid -> normal weighted sum
  expect_equal(result[1], (0.8 * 0.4 + 0.5 * 0.6) / (0.4 + 0.6))

  # Row 2: v1 is NA (=0), v2 is valid -> (0 * 0.4 + 0.6 * 0.6) / 1.0
  expect_equal(result[2], (0 * 0.4 + 0.6 * 0.6) / (0.4 + 0.6))

  # Row 3: v1 is valid, v2 is NA (=0) -> (0.9 * 0.4 + 0 * 0.6) / 1.0
  expect_equal(result[3], (0.9 * 0.4 + 0 * 0.6) / (0.4 + 0.6))
})

test_that("compute_weighted_sum handles all NA", {
  # All components are NA -> treated as 0, result is 0
  v1 <- c(NA, 0.8)
  v2 <- c(NA, 0.6)
  w <- c(0.5, 0.5)

  result <- compute_weighted_sum(v1, v2, weights = w)

  # Row 1: all NA (=0) -> (0 * 0.5 + 0 * 0.5) = 0
  expect_equal(result[1], 0)

  # Row 2: both valid -> normal weighted sum
  expect_equal(result[2], (0.8 * 0.5 + 0.6 * 0.5) / (0.5 + 0.5))
})

test_that("compute_weighted_sum MS1-only with biological score", {
  # Real scenario: MS1-only hits have NA initial score
  # NA is treated as 0 in the weighted sum calculation
  bio_score <- c(0.9, 0.7, 0.5) # good, medium, bad
  initial_score <- c(0.8, NA, NA) # MS1-only have NA (=0)

  result <- compute_weighted_sum(
    bio_score,
    initial_score,
    weights = c(1.0, 1.0) # equal weights (0.5, 0.5 normalized)
  )

  # Row 1: both valid
  expect_equal(result[1], (0.9 + 0.8) / 2)

  # Row 2: MS1-only, NA treated as 0 -> (0.7 * 0.5 + 0 * 0.5) = 0.35
  expect_equal(result[2], (0.7 * 0.5 + 0 * 0.5))

  # Row 3: MS1-only, NA treated as 0 -> (0.5 * 0.5 + 0 * 0.5) = 0.25
  expect_equal(result[3], (0.5 * 0.5 + 0 * 0.5))
})

test_that("compute_weighted_sum with unequal weights and NA", {
  # Biological score has more weight (0.6) than initial (0.4)
  bio_score <- c(0.8, NA)
  initial_score <- c(0.6, 0.7)
  w_bio <- 0.6
  w_init <- 0.4

  result <- compute_weighted_sum(
    bio_score,
    initial_score,
    weights = c(w_bio, w_init)
  )

  # Row 1: both valid
  expect_equal(result[1], (0.8 * 0.6 + 0.6 * 0.4) / 1.0)

  # Row 2: bio is NA (=0), init valid -> (0 * 0.6 + 0.7 * 0.4)
  expect_equal(result[2], (0 * 0.6 + 0.7 * 0.4) / 1.0)
})

test_that("compute_weighted_sum real-world MS1 scenario", {
  # Real scenario from tima pipeline:
  # - candidate_score_pseudo_initial is NA for MS1-only hits (no MS2 spectrum)
  # - score_weighted_bio combines biological score with initial score
  # - NA is treated as 0, contributing 0 to the weighted sum

  # Scenario: 3 candidates - 2 MS1-only, 1 with MS2
  bio_scores <- c(0.85, 0.75, 0.95)
  initial_scores <- c(0.70, NA_real_, NA_real_) # MS1-only have NA (=0)

  # Weights from weight_bio
  w_bio <- 1.0
  w_spec <- 1.0

  result <- compute_weighted_sum(
    bio_scores,
    initial_scores,
    weights = c(w_bio, w_spec)
  )

  # Candidate 1: both scores valid
  # Expected: (0.85*0.5 + 0.70*0.5) = 0.775
  expect_equal(result[1], 0.775)

  # Candidate 2: MS1-only, initial is NA (=0)
  # Expected: (0.75*0.5 + 0*0.5) = 0.375
  expect_equal(result[2], 0.375)

  # Candidate 3: MS1-only, initial is NA (=0)
  # Expected: (0.95*0.5 + 0*0.5) = 0.475
  expect_equal(result[3], 0.475)

  # Verify no NAs in result
  expect_true(!any(is.na(result)))
})
