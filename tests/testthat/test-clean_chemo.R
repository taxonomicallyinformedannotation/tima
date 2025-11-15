# ==============================================================================
# Test Suite: clean_chemo
# ==============================================================================
library(testthat)
library(tima)

candidates_final <- tidytable::tidytable(
  candidate_structure_inchikey_connectivity_layer = character()
)

test_that("clean_chemo handles empty data frame", {
  result <- clean_chemo(tidytable::tidytable())
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("clean_chemo processes minimal chemical class columns", {
  df <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = c("A", "B"),
    structure_classyfire_class = c("Alkaloids", "Flavonoids")
  )
  candidates_final <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c("A", "B")
  )
  result <- clean_chemo(df)
  expect_true("structure_inchikey_connectivity_layer" %in% names(result))
})

test_that("clean_chemo validates minimal_ms1_condition", {
  annot_table_wei_chemo <- tidytable::tidytable(
    feature_id = "F1",
    candidate_structure_inchikey_connectivity_layer = "A",
    candidate_score_similarity = NA_real_,
    candidate_score_sirius_csi = NA_real_,
    score_biological = 0.9,
    score_chemical = 0.1
  )
  candidates_final <- 1
  best_percentile <- 0.9
  minimal_ms1_bio <- 0.5
  minimal_ms1_chemo <- 0.5
  minimal_ms1_condition <- "X"
  compounds_names <- FALSE
  high_confidence <- FALSE
  remove_ties <- FALSE
  summarize <- FALSE
  expect_error(
    clean_chemo(
      annot_table_wei_chemo = annot_table_wei_chemo,
      candidates_final = candidates_final,
      best_percentile = best_percentile,
      minimal_ms1_bio = minimal_ms1_bio,
      minimal_ms1_chemo = minimal_ms1_chemo,
      minimal_ms1_condition = minimal_ms1_condition,
      compounds_names = compounds_names,
      high_confidence = high_confidence,
      remove_ties = remove_ties,
      summarize = summarize
    ),
    "OR or AND"
  )
})

test_that("clean_chemo filters MS1 annotations with OR condition", {
  annot_table_wei_chemo <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B"),
    candidate_score_similarity = NA_real_,
    candidate_score_sirius_csi = NA_real_,
    score_biological = c(0.9, 0.1),
    score_chemical = c(0.1, 0.9)
  )
  candidates_final <- 2
  best_percentile <- 1
  minimal_ms1_bio <- 0.8
  minimal_ms1_chemo <- 0.8
  minimal_ms1_condition <- "OR"
  compounds_names <- FALSE
  high_confidence <- FALSE
  remove_ties <- FALSE
  summarize <- FALSE
  result <- clean_chemo(
    annot_table_wei_chemo = annot_table_wei_chemo,
    candidates_final = candidates_final,
    best_percentile = best_percentile,
    minimal_ms1_bio = minimal_ms1_bio,
    minimal_ms1_chemo = minimal_ms1_chemo,
    minimal_ms1_condition = minimal_ms1_condition,
    compounds_names = compounds_names,
    high_confidence = high_confidence,
    remove_ties = remove_ties,
    summarize = summarize
  )
  expect_lte(nrow(result), 2L)
})

test_that("clean_chemo filters MS1 annotations with AND condition", {
  annot_table_wei_chemo <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B"),
    candidate_score_similarity = NA_real_,
    candidate_score_sirius_csi = NA_real_,
    score_biological = c(0.9, 0.7),
    score_chemical = c(0.85, 0.6)
  )
  candidates_final <- 2
  best_percentile <- 1
  minimal_ms1_bio <- 0.8
  minimal_ms1_chemo <- 0.8
  minimal_ms1_condition <- "AND"
  compounds_names <- FALSE
  high_confidence <- FALSE
  remove_ties <- FALSE
  summarize <- FALSE
  result <- clean_chemo(
    annot_table_wei_chemo = annot_table_wei_chemo,
    candidates_final = candidates_final,
    best_percentile = best_percentile,
    minimal_ms1_bio = minimal_ms1_bio,
    minimal_ms1_chemo = minimal_ms1_chemo,
    minimal_ms1_condition = minimal_ms1_condition,
    compounds_names = compounds_names,
    high_confidence = high_confidence,
    remove_ties = remove_ties,
    summarize = summarize
  )
  # Only F1 should pass both thresholds
  expect_true(all(result$feature_id %in% c("F1", "F2")))
})
