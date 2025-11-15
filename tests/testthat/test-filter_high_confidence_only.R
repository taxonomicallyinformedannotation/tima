# ==============================================================================
# Test Suite: filter_high_confidence_only
# ==============================================================================
library(testthat)
library(tima)

test_that("filter_high_confidence_only handles empty input", {
  empty <- tidytable::tidytable()
  result <- filter_high_confidence_only(empty)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

# test_that("filter_high_confidence_only filters by boolean column", {
#   df <- tidytable::tidytable(
#     feature_id = c("F1", "F2", "F3"),
#     annotation_high_confidence = c(TRUE, FALSE, TRUE)
#   )
#   result <- filter_high_confidence_only(df)
#   expect_equal(nrow(result), 2L)
#   expect_setequal(result$feature_id, c("F1", "F3"))
# })

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
  expect_error(filter_high_confidence_only(df, error_rt_max = 0), "positive")
})

test_that("filter_high_confidence_only applies RT error filter", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    score_biological = c(0.9, 0.1, 0.1),
    candidate_score_pseudo_initial = c(0.1, 0.96, 0.1),
    score_weighted_chemo = c(0.1, 0.1, 0.8),
    candidate_structure_error_rt = c(0.05, 0.2, NA)
  )
  result <- filter_high_confidence_only(df, error_rt_max = 0.1)
  # F2 should be removed due to RT error 0.2; F1 passes bio; F3 passes chemo with NA RT
  expect_setequal(result$feature_id, c("F1", "F3"))
})
