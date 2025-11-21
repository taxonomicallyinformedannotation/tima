# Test Suite: clean_chemo ----

library(testthat)

# Test Fixtures ----

#' Create minimal annotation table for clean_chemo testing
#' @keywords internal
create_test_annotation_table <- function(n_features = 3, n_per_feature = 5) {
  tidytable::tidytable(
    feature_id = rep(sprintf("F%03d", seq_len(n_features)), each = n_per_feature),
    candidate_structure_inchikey_connectivity_layer = replicate(
      n_features * n_per_feature,
      paste(sample(LETTERS, 14, TRUE), collapse = "")
    ),
    score_weighted_chemo = runif(n_features * n_per_feature, 0, 1),
    score_biological = runif(n_features * n_per_feature, 0, 1),
    score_chemical = runif(n_features * n_per_feature, 0, 1),
    candidate_score_pseudo_initial = runif(n_features * n_per_feature, 0, 1),
    candidate_score_similarity = c(
      runif(n_features * (n_per_feature - 2), 0, 1),
      rep(NA, n_features * 2)
    ), # Some with MS2
    candidate_score_sirius_csi = NA_real_,
    candidate_structure_name = paste("Compound", seq_len(n_features * n_per_feature))
  )
}

# Unit Tests: Helper Functions ----

test_that("validate_clean_chemo_inputs accepts valid inputs", {
  ann <- create_test_annotation_table()

  expect_silent(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = ann,
      candidates_final = 5,
      best_percentile = 0.9,
      minimal_ms1_bio = 0.1,
      minimal_ms1_chemo = 0.1,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE
    )
  )
})

test_that("validate_clean_chemo_inputs rejects invalid data frame", {
  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = 123,
      candidates_final = 5,
      best_percentile = 0.9,
      minimal_ms1_bio = 0.1,
      minimal_ms1_chemo = 0.1,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE
    ),
    "must be a data frame"
  )
})

test_that("validate_clean_chemo_inputs rejects invalid candidates_final", {
  ann <- create_test_annotation_table()

  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = ann,
      candidates_final = 0,
      best_percentile = 0.9,
      minimal_ms1_bio = 0.1,
      minimal_ms1_chemo = 0.1,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE
    ),
    "candidates_final must be a positive integer"
  )

  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = ann,
      candidates_final = -5,
      best_percentile = 0.9,
      minimal_ms1_bio = 0.1,
      minimal_ms1_chemo = 0.1,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE
    ),
    "candidates_final must be a positive integer"
  )
})

test_that("validate_clean_chemo_inputs rejects invalid percentiles", {
  ann <- create_test_annotation_table()

  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = ann,
      candidates_final = 5,
      best_percentile = 1.5,
      minimal_ms1_bio = 0.1,
      minimal_ms1_chemo = 0.1,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE
    ),
    "best_percentile must be between 0 and 1"
  )
})

test_that("validate_clean_chemo_inputs rejects invalid MS1 scores", {
  ann <- create_test_annotation_table()

  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = ann,
      candidates_final = 5,
      best_percentile = 0.9,
      minimal_ms1_bio = -0.1,
      minimal_ms1_chemo = 0.1,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE
    ),
    "minimal_ms1_bio must be between 0 and 1"
  )
})

test_that("validate_clean_chemo_inputs rejects invalid condition", {
  ann <- create_test_annotation_table()

  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = ann,
      candidates_final = 5,
      best_percentile = 0.9,
      minimal_ms1_bio = 0.1,
      minimal_ms1_chemo = 0.1,
      minimal_ms1_condition = "INVALID",
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE
    ),
    "minimal_ms1_condition must be 'OR' or 'AND'"
  )
})

test_that("validate_clean_chemo_inputs rejects invalid logical parameters", {
  ann <- create_test_annotation_table()

  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = ann,
      candidates_final = 5,
      best_percentile = 0.9,
      minimal_ms1_bio = 0.1,
      minimal_ms1_chemo = 0.1,
      minimal_ms1_condition = "OR",
      compounds_names = "yes",
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE
    ),
    "must be logical"
  )
})

test_that("filter_ms1_annotations filters with OR condition", {
  ann <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3", "F4"),
    score_biological = c(0.9, 0.1, 0.9, 0.1),
    score_chemical = c(0.1, 0.9, 0.1, 0.1),
    candidate_score_similarity = c(NA, NA, NA, 0.8),
    candidate_score_sirius_csi = c(NA, NA, NA, NA)
  )

  result <- filter_ms1_annotations(
    annot_table_wei_chemo = ann,
    minimal_ms1_bio = 0.5,
    minimal_ms1_chemo = 0.5,
    minimal_ms1_condition = "OR"
  )

  # F1: bio=0.9 >= 0.5 OR chem=0.1 < 0.5 → PASS (bio meets)
  # F2: bio=0.1 < 0.5 OR chem=0.9 >= 0.5 → PASS (chem meets)
  # F3: bio=0.9 >= 0.5 OR chem=0.1 < 0.5 → PASS (bio meets)
  # F4: has MS2 (similarity=0.8) → PASS
  expect_equal(nrow(result), 4)
})

test_that("filter_ms1_annotations filters with AND condition", {
  ann <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    score_biological = c(0.9, 0.1, 0.9),
    score_chemical = c(0.1, 0.9, 0.9),
    candidate_score_similarity = c(NA, NA, NA),
    candidate_score_sirius_csi = c(NA, NA, NA)
  )

  result <- filter_ms1_annotations(
    annot_table_wei_chemo = ann,
    minimal_ms1_bio = 0.5,
    minimal_ms1_chemo = 0.5,
    minimal_ms1_condition = "AND"
  )

  # F1: bio=0.9 AND chem=0.1 → FAIL
  # F2: bio=0.1 AND chem=0.9 → FAIL
  # F3: bio=0.9 AND chem=0.9 → PASS
  expect_equal(nrow(result), 1)
  expect_equal(result$feature_id, "F3")
})

test_that("rank_and_deduplicate ranks candidates correctly", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B", "C"),
    score_weighted_chemo = c("0.9", "0.8", "0.7"),
    candidate_score_pseudo_initial = c("0.85", "0.75", "0.80")
  )

  result <- rank_and_deduplicate(df)

  expect_true("rank_initial" %in% names(result))
  expect_true("rank_final" %in% names(result))
  expect_equal(nrow(result), 3) # All unique structures kept
})

test_that("rank_and_deduplicate removes duplicate structures", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1"),
    candidate_structure_inchikey_connectivity_layer = c("A", "A", "B"),
    score_weighted_chemo = c("0.9", "0.8", "0.7"),
    candidate_score_pseudo_initial = c("0.85", "0.75", "0.80")
  )

  result <- rank_and_deduplicate(df)

  # Should keep only best scoring instance of "A" and "B"
  expect_equal(nrow(result), 2)
  expect_true(all(c("A", "B") %in% result$candidate_structure_inchikey_connectivity_layer))
})

test_that("apply_percentile_filter keeps top percentile", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1", "F1"),
    score_weighted_chemo = c("1.0", "0.9", "0.5", "0.1")
  )

  result <- apply_percentile_filter(df, best_percentile = 0.9)

  # 0.9 * 1.0 = 0.9 threshold
  # Keep: 1.0, 0.9
  # Filter: 0.5, 0.1
  expect_equal(nrow(result), 2)
})

test_that("count_candidates counts correctly", {
  df_ranked <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1", "F2", "F2")
  )

  df_percentile <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F2")
  )

  result <- count_candidates(df_ranked, df_percentile)

  expect_equal(nrow(result), 2)
  expect_true(all(c("feature_id", "candidates_evaluated", "candidates_best") %in% names(result)))

  f1 <- result[result$feature_id == "F1", ]
  expect_equal(f1$candidates_evaluated, 3)
  expect_equal(f1$candidates_best, 2)
})

# Integration Tests ----

# test_that("clean_chemo handles empty annotation table", {
#   empty <- tidytable::tidytable()
#
#   result <- clean_chemo(
#     annot_table_wei_chemo = empty,
#     components_table = tidytable::tidytable(),
#     features_table = tidytable::tidytable(),
#     structure_organism_pairs_table = tidytable::tidytable(),
#     candidates_final = 5,
#     best_percentile = 0.9,
#     minimal_ms1_bio = 0.1,
#     minimal_ms1_chemo = 0.1,
#     minimal_ms1_condition = "OR",
#     compounds_names = TRUE,
#     high_confidence = FALSE,
#     remove_ties = FALSE,
#     summarize = FALSE
#   )
#
#   expect_type(result, "list")
#   expect_true(all(vapply(result, is.data.frame, logical(1))))
#   expect_equal(nrow(result$full), 0)
#   expect_equal(nrow(result$filtered), 0)
#   expect_equal(nrow(result$mini), 0)
# })

# Edge Cases ----

test_that("remove_compound_names removes names when compounds_names=FALSE", {
  results_list <- list(
    full = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_name = "Compound A"
    ),
    filtered = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_name = "Compound A"
    ),
    mini = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_name = "Compound A"
    )
  )

  result <- remove_compound_names(results_list, compounds_names = FALSE)

  expect_false("candidate_structure_name" %in% names(result$full))
  expect_false("candidate_structure_name" %in% names(result$filtered))
  expect_false("candidate_structure_name" %in% names(result$mini))
})

test_that("remove_compound_names keeps names when compounds_names=TRUE", {
  results_list <- list(
    full = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_name = "Compound A"
    ),
    filtered = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_name = "Compound A"
    ),
    mini = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_name = "Compound A"
    )
  )

  result <- remove_compound_names(results_list, compounds_names = TRUE)

  expect_true("candidate_structure_name" %in% names(result$full))
  expect_true("candidate_structure_name" %in% names(result$filtered))
  expect_true("candidate_structure_name" %in% names(result$mini))
})
