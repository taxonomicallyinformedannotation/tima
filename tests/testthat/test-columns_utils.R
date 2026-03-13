# Test Suite: columns_utils ----
# Tests columns_model(), clean_collapse(), fake_annotations_columns(),
# fake_sop_columns() — all previously untested.

library(testthat)

# ---- columns_model -----------------------------------------------------------

test_that("columns_model returns a named list with expected keys", {
  model <- columns_model()
  expect_type(model, "list")
  expected_keys <- c(
    "features_columns",
    "features_calculated_columns",
    "candidates_calculated_columns",
    "candidates_sirius_for_columns",
    "candidates_sirius_str_columns",
    "candidates_spectra_columns",
    "candidates_structures_columns",
    "components_columns",
    "rank_columns",
    "score_columns"
  )
  expect_named(model, expected_keys, ignore.order = TRUE)
})

test_that("columns_model feature columns contain core identifiers", {
  m <- columns_model()
  expect_true("feature_id" %in% m$features_columns)
  expect_true("feature_mz" %in% m$features_columns)
  expect_true("feature_rt" %in% m$features_columns)
})

test_that("columns_model tag column is present in candidates_structures_columns", {
  m <- columns_model()
  expect_true(
    "candidate_structure_tag" %in%
      m$candidates_structures_columns
  )
})

test_that("columns_model score columns include all primary scores", {
  m <- columns_model()
  expect_true(all(
    c("score_biological", "score_chemical", "score_final") %in% m$score_columns
  ))
})

# ---- clean_collapse ----------------------------------------------------------

test_that("clean_collapse collapses unique values per group with separator", {
  df <- tidytable::tidytable(
    group = c("A", "A", "B"),
    val = c("x", "y", "x")
  ) |>
    tidytable::group_by(group)

  out <- clean_collapse(df, cols = "val", separator = " | ")
  expect_equal(nrow(out), 2L)
  a_row <- out[out$group == "A", ]$val
  expect_true(grepl("x", a_row) && grepl("y", a_row))
})

test_that("clean_collapse drops NA values before collapsing", {
  df <- tidytable::tidytable(
    group = c("A", "A"),
    val = c("x", NA_character_)
  ) |>
    tidytable::group_by(group)

  out <- clean_collapse(df, cols = "val")
  expect_equal(trimws(out$val), "x")
})

test_that("clean_collapse deduplicates repeated values", {
  df <- tidytable::tidytable(
    group = c("A", "A", "A"),
    val = c("x", "x", "x")
  ) |>
    tidytable::group_by(group)

  out <- clean_collapse(df, cols = "val", separator = " $ ")
  # No separator should appear since only one unique value
  expect_false(grepl("$", out$val, fixed = TRUE))
  expect_equal(out$val, "x")
})

# ---- fake_annotations_columns ------------------------------------------------

test_that("fake_annotations_columns returns a 1-row NA data frame", {
  out <- fake_annotations_columns()
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1L)
  expect_true("feature_id" %in% names(out))
  expect_true("candidate_structure_tag" %in% names(out))
  expect_true(all(is.na(out)))
})

# ---- fake_sop_columns --------------------------------------------------------

test_that("fake_sop_columns returns a 1-row NA tidytable with tag column", {
  out <- fake_sop_columns()
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1L)
  expect_true("structure_tag" %in% names(out))
  expect_true("organism_name" %in% names(out))
  expect_true("structure_inchikey" %in% names(out))
  expect_true(is.na(out$structure_tag))
})
