library(testthat)
sample_candidates_per_group <- sample_candidates_per_group
remove_compound_names <- remove_compound_names
coerce_score_columns <- coerce_score_columns

# ── sample_candidates_per_group ───────────────────────────────────────────────

test_that("sample_candidates_per_group returns empty list for 0-row input", {
  df <- tidytable::tidytable(
    feature_id = character(),
    candidate_adduct = character(),
    rank_final = integer()
  )
  result <- sample_candidates_per_group(df, max_per_score = 3L)
  expect_equal(nrow(result$df), 0L)
  expect_equal(result$n_sampled_features, 0L)
})

test_that("sample_candidates_per_group passes through untied rows unchanged", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_adduct = c("[M+H]+", "[M+H]+"),
    rank_final = c(1L, 1L),
    score_weighted_chemo = c(0.9, 0.8),
    candidate_score_pseudo_initial = c(0.5, 0.4)
  )
  result <- sample_candidates_per_group(df, max_per_score = 5L)
  expect_equal(nrow(result$df), 2L)
  expect_equal(result$n_sampled_features, 0L)
})

test_that("sample_candidates_per_group keeps all tied rows below cap", {
  df <- tidytable::tidytable(
    feature_id = rep("F1", 3L),
    candidate_adduct = rep("[M+H]+", 3L),
    rank_final = rep(1L, 3L),
    score_weighted_chemo = c(0.9, 0.9, 0.9),
    candidate_score_pseudo_initial = c(0.5, 0.5, 0.5)
  )
  result <- sample_candidates_per_group(df, max_per_score = 5L)
  # All 3 tied rows below cap of 5 -> keep all
  expect_equal(nrow(result$df), 3L)
})

test_that("sample_candidates_per_group samples down when above cap", {
  df <- tidytable::tidytable(
    feature_id = rep("F1", 6L),
    candidate_adduct = rep("[M+H]+", 6L),
    rank_final = rep(1L, 6L),
    score_weighted_chemo = rep(0.9, 6L),
    candidate_score_pseudo_initial = rep(0.5, 6L)
  )
  result <- sample_candidates_per_group(df, max_per_score = 3L, seed = 42L)
  # 6 ties > cap(3) -> sample to 3
  expect_equal(nrow(result$df), 3L)
  expect_equal(result$n_sampled_features, 1L)
})

test_that("sample_candidates_per_group stores annotation notes for sampled groups", {
  df <- tidytable::tidytable(
    feature_id = rep("F1", 5L),
    candidate_adduct = rep("[M+H]+", 5L),
    rank_final = rep(1L, 5L),
    score_weighted_chemo = rep(0.8, 5L),
    candidate_score_pseudo_initial = rep(0.5, 5L)
  )
  result <- sample_candidates_per_group(df, max_per_score = 2L, seed = 1L)
  expect_true(nrow(result$annotation_notes) >= 1L)
})

# ── remove_compound_names ──────────────────────────────────────────────────────

test_that("remove_compound_names strips name column when compounds_names=FALSE", {
  tbl <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_name = c("Compound A", "Compound B"),
    score = c(0.8, 0.7)
  )
  results_list <- list(full = tbl, filtered = tbl, mini = tbl)

  result <- remove_compound_names(results_list, compounds_names = FALSE)
  expect_false("candidate_structure_name" %in% names(result$full))
  expect_false("candidate_structure_name" %in% names(result$filtered))
  expect_false("candidate_structure_name" %in% names(result$mini))
})

test_that("remove_compound_names keeps name column when compounds_names=TRUE", {
  tbl <- tidytable::tidytable(
    feature_id = c("F1"),
    candidate_structure_name = c("Compound A"),
    score = c(0.8)
  )
  results_list <- list(full = tbl, filtered = tbl, mini = tbl)

  result <- remove_compound_names(results_list, compounds_names = TRUE)
  expect_true("candidate_structure_name" %in% names(result$full))
})

# ── coerce_score_columns ──────────────────────────────────────────────────────

test_that("coerce_score_columns converts character score columns to numeric", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    score_biological = c("0.8", "0.5"),
    score_chemical = c("0.7", "0.3"),
    candidate_score_similarity = c("0.9", NA_character_)
  )
  result <- coerce_score_columns(df)
  expect_true(is.numeric(result$score_biological))
  expect_true(is.numeric(result$score_chemical))
  expect_true(is.numeric(result$candidate_score_similarity))
  expect_equal(result$score_biological, c(0.8, 0.5))
})

test_that("coerce_score_columns is a no-op when no score columns present", {
  df <- tidytable::tidytable(
    feature_id = c("F1"),
    other_col = c("abc")
  )
  result <- coerce_score_columns(df)
  expect_equal(names(result), names(df))
})

test_that("coerce_score_columns handles all expected score column names", {
  cols <- c(
    "score_biological",
    "score_chemical",
    "score_weighted_chemo",
    "candidate_score_pseudo_initial",
    "candidate_score_similarity",
    "candidate_score_similarity_forward",
    "candidate_score_similarity_reverse",
    "candidate_score_sirius_csi",
    "candidate_score_sirius_confidence"
  )
  df_vals <- setNames(as.list(as.character(seq_along(cols) * 0.1)), cols)
  df <- tidytable::as_tidytable(as.data.frame(
    df_vals,
    stringsAsFactors = FALSE
  ))
  result <- coerce_score_columns(df)
  for (col in cols) {
    expect_true(is.numeric(result[[col]]), info = col)
  }
})
