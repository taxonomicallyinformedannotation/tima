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

test_that("compute_weighted_sum treats missing evidence as zero", {
  v1 <- c(0.8, NA, 0.9) # biological score
  v2 <- c(0.5, 0.6, NA) # initial/spectral score
  w <- c(0.4, 0.6)

  result <- compute_weighted_sum(v1, v2, weights = w)

  expect_equal(result[1], (0.8 * 0.4 + 0.5 * 0.6) / (0.4 + 0.6))
  expect_equal(result[2], 0.36)
  expect_equal(result[3], 0.36)
})

test_that("compute_weighted_sum returns zero for all-NA rows", {
  v1 <- c(NA, 0.8)
  v2 <- c(NA, 0.6)
  w <- c(0.5, 0.5)

  result <- compute_weighted_sum(v1, v2, weights = w)

  expect_equal(result[1], 0)
  expect_equal(result[2], (0.8 * 0.5 + 0.6 * 0.5) / (0.5 + 0.5))
})

test_that("compute_weighted_sum returns NA when all weights are zero", {
  result <- compute_weighted_sum(c(1, 2), c(3, 4), weights = c(0, 0))

  expect_true(all(is.na(result)))
})

test_that("compute_weighted_components computes evidence coverage", {
  bio_score <- c(0.9, 0.9, 0.9) # same bio score
  initial_score <- c(0.9, NA, NA) # MS1-only have NA

  components <- compute_weighted_components(
    bio_score,
    initial_score,
    weights = c(1.0, 1.0)
  )

  expect_equal(components$score[1], 0.9)
  expect_equal(components$score[2], 0.45)
  expect_equal(components$score[3], 0.45)
  expect_equal(components$coverage, c(1.0, 0.5, 0.5))
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
  expect_equal(result[2], 0.28)
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

  expect_equal(result[2], 0.375)
  expect_equal(result[3], 0.475)

  # No NAs (each row has at least one non-NA component)
  expect_true(!anyNA(result))

  expect_equal(
    compute_weighted_components(
      bio_scores,
      initial_scores,
      weights = c(w_bio, w_spec)
    )$coverage,
    c(1.0, 0.5, 0.5)
  )
})

test_that("compute_weighted_components supports na_prior shrinkage and compute_rt_mz_penalty", {
  v1 <- c(0.8, 0.4, NA)
  v2 <- c(0.6, NA, 0.6)
  res_mean <- compute_weighted_components(
    v1,
    v2,
    weights = c(0.5, 0.5),
    na_prior = "mean"
  )
  expect_equal(res_mean$score, c(0.7, 0.5, 0.6))
  expect_equal(res_mean$coverage, c(1.0, 0.5, 0.5))

  res_vec <- compute_weighted_sum(
    v1,
    v2,
    weights = c(0.5, 0.5),
    na_prior = c(0.2, 0.4)
  )
  expect_equal(res_vec, c(0.7, 0.4, 0.4))

  # Scalar recycling and all-NA vector with na_prior = "mean"
  res_recycle_all_na <- compute_weighted_components(
    0.8,
    c(NA_real_, NA_real_),
    weights = c(0.5, 0.5),
    na_prior = "mean"
  )
  expect_equal(res_recycle_all_na$score, c(0.4, 0.4))
  expect_equal(res_recycle_all_na$coverage, c(0.5, 0.5))

  pen <- compute_rt_mz_penalty(
    error_rt = c(0, 0.05, NA),
    error_mz = c(0, 0.005, 0),
    rt_half_min = 0.05,
    mz_half_da = 0.005
  )
  expect_equal(pen, c(1.0, 0.25, 1.0))
  expect_equal(compute_rt_mz_penalty(), numeric(0L))
  expect_error(
    compute_rt_mz_penalty(error_rt = 0.1, rt_half_min = 0),
    class = "tima_validation_error"
  )
  expect_error(
    compute_rt_mz_penalty(error_mz = 0.01, mz_half_da = -1),
    class = "tima_validation_error"
  )
  expect_error(
    compute_weighted_components(v1, v2, weights = c(1, 1), na_prior = -0.1),
    class = "tima_validation_error"
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
