# Test Suite: extract_chemo_scores ----

library(testthat)

.chemo_wide <- function() {
  tidytable::tidytable(
    feature_id = c("F1", "F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("AAA", "BBB", "AAA"),
    score_chemical = c(0.8, 0.4, 0.6),
    score_weighted_chemo = c(0.5, 0.2, 0.3),
    score_bio = c(0.1, 0.9, 0.7),
    other_column = c(10, 20, 30)
  )
}

test_that("extract_chemo_scores narrows the weight_chemo output to four columns", {
  out <- extract_chemo_scores(.chemo_wide())

  expect_named(
    out,
    c(
      "feature_id",
      "candidate_structure_inchikey_connectivity_layer",
      "score_chemical",
      "score_weighted_chemo"
    )
  )
  expect_equal(nrow(out), 3L)
  # Row order and values are preserved verbatim.
  expect_equal(
    as.data.frame(out),
    as.data.frame(.chemo_wide())[, names(out)]
  )
})

test_that("extract_chemo_scores returns a typed empty table for an empty input", {
  out <- extract_chemo_scores(.chemo_wide()[0, ])

  expect_equal(nrow(out), 0L)
  expect_named(
    out,
    c(
      "feature_id",
      "candidate_structure_inchikey_connectivity_layer",
      "score_chemical",
      "score_weighted_chemo"
    )
  )
  # The empty schema is typed, so downstream tidyselect/numeric code does not
  # have to special-case a 0-column data.frame.
  expect_type(out$feature_id, "character")
  expect_type(
    out$candidate_structure_inchikey_connectivity_layer,
    "character"
  )
  expect_type(out$score_chemical, "double")
  expect_type(out$score_weighted_chemo, "double")
  expect_length(out$score_chemical, 0L)
})

test_that("extract_chemo_scores still rejects a non-data-frame input", {
  expect_error(extract_chemo_scores("not a table"), "data frame")
  expect_error(extract_chemo_scores(NULL), "data frame")
})
