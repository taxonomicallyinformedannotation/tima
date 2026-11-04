# Test Suite: extract_chemo_scores ----

library(testthat)

test_that("extract_chemo_scores validates dataframe input and handles zero-row tables", {
  expect_error(
    extract_chemo_scores(NULL),
    class = "tima_validation_error"
  )
  expect_error(
    extract_chemo_scores("not_a_df"),
    class = "tima_validation_error"
  )

  empty_df <- data.frame(
    feature_id = character(0),
    other_col = numeric(0)
  )
  res_empty <- extract_chemo_scores(empty_df)
  expect_s3_class(res_empty, "tidytable")
  expect_identical(nrow(res_empty), 0L)
  expect_identical(
    names(res_empty),
    c(
      "feature_id",
      "candidate_structure_inchikey_connectivity_layer",
      "score_chemical",
      "score_weighted_chemo"
    )
  )
  expect_type(res_empty$feature_id, "character")
  expect_type(
    res_empty$candidate_structure_inchikey_connectivity_layer,
    "character"
  )
  expect_type(res_empty$score_chemical, "double")
  expect_type(res_empty$score_weighted_chemo, "double")
})

test_that("extract_chemo_scores extracts narrow columns and errors when required columns are missing", {
  wide_df <- tidytable::tidytable(
    extra_before = c("x", "y"),
    score_weighted_chemo = c(0.85, 0.40),
    feature_id = c("F1", "F2"),
    score_chemical = c(0.75, 0.25),
    candidate_structure_inchikey_connectivity_layer = c("IK1", "IK2"),
    extra_after = c(1L, 2L)
  )

  res <- extract_chemo_scores(wide_df)
  expect_s3_class(res, "tidytable")
  expect_identical(
    names(res),
    c(
      "feature_id",
      "candidate_structure_inchikey_connectivity_layer",
      "score_chemical",
      "score_weighted_chemo"
    )
  )
  expect_identical(res$feature_id, c("F1", "F2"))
  expect_identical(
    res$candidate_structure_inchikey_connectivity_layer,
    c("IK1", "IK2")
  )
  expect_equal(res$score_chemical, c(0.75, 0.25), tolerance = 1e-12)
  expect_equal(res$score_weighted_chemo, c(0.85, 0.40), tolerance = 1e-12)

  # Missing required column falls through to tidytable::select and errors cleanly
  incomplete_df <- tidytable::tidytable(
    feature_id = "F1",
    score_chemical = 0.5
  )
  expect_error(
    extract_chemo_scores(incomplete_df),
    class = "vctrs_error_subscript_oob"
  )
})
