# Test Suite: clean_collapse ----

library(testthat)

test_that("clean_collapse collapses specified columns", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F2"),
    value = c(1, 2, 3)
  )
  grouped <- tidytable::group_by(df, feature_id)
  result <- clean_collapse(grouped_df = grouped, cols = c("value"))
  expect_s3_class(result, "data.frame")
  expect_true("value" %in% names(result))
})

test_that("clean_collapse removes NA and collapses unique values", {
  df <- tidytable::tidytable(group = c("G", "G", "G"), value = c("A", "A", NA))
  grouped <- tidytable::group_by(df, group)
  result <- clean_collapse(grouped_df = grouped, cols = c("value"))
  expect_equal(result$value, "A")
})
