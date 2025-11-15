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

test_that("filter_high_confidence_only filters by boolean column", {
  df <- tidytable::tidytable(
    feature_id = c("F1","F2","F3"),
    annotation_high_confidence = c(TRUE, FALSE, TRUE)
  )
  result <- filter_high_confidence_only(df)
  expect_equal(nrow(result), 2L)
  expect_setequal(result$feature_id, c("F1","F3"))
})

