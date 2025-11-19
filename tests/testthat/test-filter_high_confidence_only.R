# Test Suite: filter_high_confidence_only ----

library(testthat)

test_that("filter_high_confidence_only handles empty input", {
  empty <- tidytable::tidytable()
  result <- filter_high_confidence_only(empty)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

# Basic score-based filtering across primary thresholds
test_that("filter_high_confidence_only keeps rows meeting any primary threshold", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    score_biological = c(0.9, 0.1, 0.1),
    candidate_score_pseudo_initial = c(0.1, 0.96, 0.1),
    score_weighted_chemo = c(0.1, 0.1, 0.8)
  )
  res <- filter_high_confidence_only(
    df,
    score_bio_min = 0.85,
    score_ini_min = 0.95,
    score_final_min = 0.75
  )
  expect_setequal(res$feature_id, c("F1", "F2", "F3"))
})

test_that("filter_high_confidence_only validates threshold ranges", {
  df <- tidytable::tidytable(
    feature_id = "F1",
    score_biological = 0.9,
    candidate_score_pseudo_initial = 0.9,
    score_weighted_chemo = 0.9
  )
  expect_error(
    filter_high_confidence_only(df, score_bio_min = -0.1),
    "between 0 and 1"
  )
  expect_error(
    filter_high_confidence_only(df, score_ini_min = 1.1),
    "between 0 and 1"
  )
  expect_error(
    filter_high_confidence_only(df, score_final_min = 2),
    "between 0 and 1"
  )
  expect_error(
    filter_high_confidence_only(df, error_rt_max = 0),
    "positive"
  )
})

# RT error filter (error in minutes). NA allowed
test_that("filter_high_confidence_only applies RT error filter (minutes)", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    score_biological = c(0.9, 0.1, 0.1),
    candidate_score_pseudo_initial = c(0.1, 0.96, 0.1),
    score_weighted_chemo = c(0.1, 0.1, 0.8),
    candidate_structure_error_rt = c(0.05, 0.2, NA_real_)
  )
  result <- filter_high_confidence_only(df, error_rt_max = 0.1)
  # F2 should be removed due to RT error 0.2; F1 passes bio; F3 passes chemo with NA RT
  expect_setequal(result$feature_id, c("F1", "F3"))
})

# Optional SIRIUS confidence and spectral similarity filters
test_that("filter_high_confidence_only applies confidence and similarity when provided", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    score_biological = c(0.2, 0.2, 0.2),
    candidate_score_pseudo_initial = c(0.2, 0.2, 0.2),
    score_weighted_chemo = c(0.9, 0.9, 0.9),
    candidate_score_sirius_confidence = c(0.95, 0.7, 0.99),
    candidate_similarity = c(0.8, 0.95, 0.6)
  )
  # With confidence >= 0.9 and similarity >= 0.75, F1 and F3 pass similarity but only F1 passes both
  res <- filter_high_confidence_only(
    df,
    confidence_sirius_min = 0.9,
    similarity_spectral_min = 0.75
  )
  expect_equal(nrow(res), 1L)
  expect_equal(res$feature_id, "F1")
})
