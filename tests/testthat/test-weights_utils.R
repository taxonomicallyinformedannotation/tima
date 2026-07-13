# Test Suite: weights_utils ----

library(testthat)

test_that("compute_weighted_sum returns weighted average at full coverage", {
  v1 <- c(0.6, 0.8)
  v2 <- c(0.4, 0.2)
  w <- c(2, 2)
  result <- compute_weighted_sum(v1, v2, weights = w)
  expected <- (v1 * 0.5 + v2 * 0.5)
  expect_equal(result, expected)

  w2 <- c(1, 3)
  result2 <- compute_weighted_sum(v1, v2, weights = w2)
  expected2 <- (v1 * 0.25 + v2 * 0.75)
  expect_equal(result2, expected2)
})

test_that("compute_weighted_sum handles three components at full coverage", {
  v1 <- c(0.5, 0.6)
  v2 <- c(0.3, 0.4)
  v3 <- c(0.2, 0.1)
  w <- c(1, 1, 1)
  result <- compute_weighted_sum(v1, v2, v3, weights = w)
  expected <- (v1 + v2 + v3) / 3
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

test_that("compute_weighted_sum rejects invalid inputs", {
  expect_error(
    compute_weighted_sum(1, 2, weights = c(1)),
    "number of value vectors must equal number of weights",
    class = "tima_validation_error"
  )

  expect_error(
    compute_weighted_sum(1, "x", weights = c(1, 1)),
    "all value vectors must be numeric",
    class = "tima_validation_error"
  )

  expect_error(
    compute_weighted_sum(1, 2, weights = c(1, -1)),
    "weights must be numeric and non-negative",
    class = "tima_validation_error"
  )
})

test_that("compute_weighted_sum returns the weighted average for partial coverage", {
  v1 <- c(0.8, NA, 0.9) # biological score
  v2 <- c(0.5, 0.6, NA) # initial/spectral score
  w <- c(0.4, 0.6)

  result <- compute_weighted_sum(v1, v2, weights = w)

  expect_equal(result[1], (0.8 * 0.4 + 0.5 * 0.6) / (0.4 + 0.6))
  expect_equal(result[2], 0.6)
  expect_equal(result[3], 0.9)
})

test_that("compute_weighted_sum returns NA for all-NA rows", {
  v1 <- c(NA, 0.8)
  v2 <- c(NA, 0.6)
  w <- c(0.5, 0.5)

  result <- compute_weighted_sum(v1, v2, weights = w)

  expect_true(is.na(result[1]))
  expect_equal(result[2], (0.8 * 0.5 + 0.6 * 0.5) / (0.5 + 0.5))
})

test_that("compute_weighted_sum returns NA when all weights are zero", {
  result <- compute_weighted_sum(c(1, 2), c(3, 4), weights = c(0, 0))

  expect_true(all(is.na(result)))
})

test_that("compute_weighted_sum exposes coverage separately for MS1-only hits", {
  bio_score <- c(0.9, 0.9, 0.9) # same bio score
  initial_score <- c(0.9, NA, NA) # MS1-only have NA

  components <- compute_weighted_components(
    bio_score,
    initial_score,
    weights = c(1.0, 1.0)
  )

  expect_equal(components$score[1], 0.9)
  expect_equal(components$score[2], 0.9)
  expect_equal(components$score[3], 0.9)
  expect_equal(components$coverage, c(1, 0.5, 0.5))
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

  expect_equal(result[1], (0.8 * 0.6 + 0.6 * 0.4) / 1.0)
  expect_equal(result[2], 0.7)
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

  expect_equal(result[2], 0.75)
  expect_equal(result[3], 0.95)

  # No NAs (each row has at least one non-NA component)
  expect_true(!anyNA(result))

  # Coverage is tracked separately and can be used as a tie-break / secondary signal.
  expect_equal(
    compute_weighted_components(
      bio_scores,
      initial_scores,
      weights = c(w_bio, w_spec)
    )$coverage,
    c(1, 0.5, 0.5)
  )
})

test_that("compute_weighted_components rejects invalid inputs", {
  expect_error(
    compute_weighted_components(1, 2, weights = c(1)),
    "number of value vectors must equal number of weights",
    class = "tima_validation_error"
  )

  expect_error(
    compute_weighted_components(1, "x", weights = c(1, 1)),
    "all value vectors must be numeric",
    class = "tima_validation_error"
  )

  expect_error(
    compute_weighted_components(1, 2, weights = c(1, -1)),
    "weights must be numeric and non-negative",
    class = "tima_validation_error"
  )
})
