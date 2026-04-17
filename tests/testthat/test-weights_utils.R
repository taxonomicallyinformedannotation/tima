# Test Suite: weights_utils ----

library(testthat)

test_that("compute_weighted_sum normalizes weights (full coverage)", {
  v1 <- c(0.6, 0.8)
  v2 <- c(0.4, 0.2)
  w <- c(2, 2)
  # Full coverage -> coverage_factor = 1.0, so pure weighted average
  result <- compute_weighted_sum(v1, v2, weights = w)
  expected <- (v1 * 0.5 + v2 * 0.5)
  expect_equal(result, expected)

  w2 <- c(1, 3)
  result2 <- compute_weighted_sum(v1, v2, weights = w2)
  expected2 <- (v1 * 0.25 + v2 * 0.75)
  expect_equal(result2, expected2)
})

test_that("compute_weighted_sum handles three components full coverage", {
  v1 <- c(0.5, 0.6)
  v2 <- c(0.3, 0.4)
  v3 <- c(0.2, 0.1)
  w <- c(1, 1, 1)
  result <- compute_weighted_sum(v1, v2, v3, weights = w)
  expected <- (v1 + v2 + v3) / 3 # full coverage -> factor = 1.0
  expect_equal(result, expected)
})

test_that("compute_weighted_sum matches original formula at full coverage", {
  score_bio <- c(0.7, 0.8, 0.9)
  score_spec <- c(0.5, 0.6, 0.4)
  w_bio <- 0.4
  w_spec <- 0.6

  result <- compute_weighted_sum(
    score_bio,
    score_spec,
    weights = c(w_bio, w_spec)
  )

  expected <- (w_bio * score_bio + w_spec * score_spec) / (w_bio + w_spec)
  expect_equal(result, expected)
})

test_that("compute_weighted_sum applies coverage discount for NA", {
  # When one component is NA, weights renormalize AND coverage discount applies
  v1 <- c(0.8, NA, 0.9) # biological score
  v2 <- c(0.5, 0.6, NA) # initial/spectral score
  w <- c(0.4, 0.6)

  result <- compute_weighted_sum(v1, v2, weights = w)

  # Row 1: both valid -> full coverage (factor=1.0) -> normal weighted avg
  expect_equal(result[1], (0.8 * 0.4 + 0.5 * 0.6) / (0.4 + 0.6))

  # Row 2: v1 NA -> coverage=0.6/1.0=0.6, factor=0.5+0.5*0.6=0.8
  # weighted_avg = 0.6, final = 0.6 * 0.8 = 0.48
  expect_equal(result[2], 0.6 * (0.5 + 0.5 * 0.6))

  # Row 3: v2 NA -> coverage=0.4/1.0=0.4, factor=0.5+0.5*0.4=0.7
  # weighted_avg = 0.9, final = 0.9 * 0.7 = 0.63
  expect_equal(result[3], 0.9 * (0.5 + 0.5 * 0.4))
})

test_that("compute_weighted_sum returns NA for all-NA rows", {
  v1 <- c(NA, 0.8)
  v2 <- c(NA, 0.6)
  w <- c(0.5, 0.5)

  result <- compute_weighted_sum(v1, v2, weights = w)

  expect_true(is.na(result[1]))
  expect_equal(result[2], (0.8 * 0.5 + 0.6 * 0.5) / (0.5 + 0.5))
})

test_that("compute_weighted_sum MS1-only scores lower than full evidence", {
  # Key property: single-evidence hits should score lower than multi-evidence
  bio_score <- c(0.9, 0.9, 0.9) # same bio score
  initial_score <- c(0.9, NA, NA) # MS1-only have NA

  result <- compute_weighted_sum(
    bio_score,
    initial_score,
    weights = c(1.0, 1.0)
  )

  # Row 1: full evidence -> 0.9
  expect_equal(result[1], 0.9)

  # Row 2-3: MS1-only -> coverage = 0.5, factor = 0.75, score = 0.9 * 0.75
  expect_equal(result[2], 0.9 * 0.75)
  expect_equal(result[3], 0.9 * 0.75)

  # Fundamental property: multi-evidence > single-evidence
  expect_true(result[1] > result[2])
})

test_that("compute_weighted_sum with unequal weights and NA", {
  bio_score <- c(0.8, NA)
  initial_score <- c(0.6, 0.7)
  w_bio <- 0.6
  w_init <- 0.4

  result <- compute_weighted_sum(
    bio_score,
    initial_score,
    weights = c(w_bio, w_init)
  )

  # Row 1: full coverage
  expect_equal(result[1], (0.8 * 0.6 + 0.6 * 0.4) / 1.0)

  # Row 2: bio NA -> coverage = 0.4/1.0 = 0.4, factor = 0.5 + 0.5*0.4 = 0.7
  # weighted_avg = 0.7, final = 0.7 * 0.7 = 0.49
  expect_equal(result[2], 0.7 * (0.5 + 0.5 * 0.4))
})

test_that("compute_weighted_sum real-world MS1 scenario", {
  bio_scores <- c(0.85, 0.75, 0.95)
  initial_scores <- c(0.70, NA_real_, NA_real_)

  w_bio <- 1.0
  w_spec <- 1.0

  result <- compute_weighted_sum(
    bio_scores,
    initial_scores,
    weights = c(w_bio, w_spec)
  )

  # Candidate 1: full evidence -> (0.85+0.70)/2 = 0.775
  expect_equal(result[1], 0.775)

  # Candidate 2: MS1-only -> coverage=0.5, factor=0.75, score=0.75*0.75=0.5625
  expect_equal(result[2], 0.75 * 0.75)

  # Candidate 3: MS1-only -> coverage=0.5, factor=0.75, score=0.95*0.75=0.7125
  expect_equal(result[3], 0.95 * 0.75)

  # No NAs (each row has at least one non-NA component)
  expect_true(!anyNA(result))

  # Key: full-evidence hit (0.775) > best MS1-only (0.7125)
  expect_true(result[1] > result[3])
})

test_that("coverage_floor = 1.0 disables coverage discount", {
  v1 <- c(0.8, NA)
  v2 <- c(0.5, 0.6)
  w <- c(1, 1)

  # With floor=1.0, coverage factor is always 1.0 (pure renormalization)
  result <- compute_weighted_sum(v1, v2, weights = w, coverage_floor = 1.0)
  expect_equal(result[1], (0.8 + 0.5) / 2)
  expect_equal(result[2], 0.6) # pure renormalized: only v2
})

test_that("coverage_floor = 0 gives maximum penalty for missing evidence", {
  v1 <- c(0.8, NA)
  v2 <- c(NA, 0.6)
  w <- c(1, 1)

  result <- compute_weighted_sum(v1, v2, weights = w, coverage_floor = 0.0)
  # Row 1: coverage=0.5, factor=0+1*0.5=0.5, score=0.8*0.5=0.4
  expect_equal(result[1], 0.4)
  # Row 2: coverage=0.5, factor=0.5, score=0.6*0.5=0.3
  expect_equal(result[2], 0.3)
})
