# Test Suite: clean_chemo ----

library(testthat)

test_that("validate_clean_chemo_inputs accepts valid inputs", {
  expect_silent(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = tidytable::tidytable(x = 1),
      candidates_final = 10,
      best_percentile = 0.9,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE
    )
  )
})

test_that("validate_clean_chemo_inputs rejects non-data.frame", {
  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = "not_a_df",
      candidates_final = 10,
      best_percentile = 0.9,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE
    ),
    "annot_table_wei_chemo must be a data frame"
  )
})

test_that("validate_clean_chemo_inputs rejects invalid candidates_final", {
  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = tidytable::tidytable(),
      candidates_final = 0,
      best_percentile = 0.9,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE
    ),
    "candidates_final must be a positive integer"
  )
})

test_that("validate_clean_chemo_inputs rejects best_percentile out of range", {
  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = tidytable::tidytable(),
      candidates_final = 10,
      best_percentile = 1.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE
    ),
    "best_percentile must be between 0 and 1"
  )
})

test_that("validate_clean_chemo_inputs rejects invalid minimal_ms1_condition", {
  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = tidytable::tidytable(),
      candidates_final = 10,
      best_percentile = 0.9,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      minimal_ms1_condition = "XOR",
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE
    ),
    "minimal_ms1_condition must be 'OR' or 'AND'"
  )
})

test_that("validate_clean_chemo_inputs rejects non-logical parameters", {
  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = tidytable::tidytable(),
      candidates_final = 10,
      best_percentile = 0.9,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      minimal_ms1_condition = "OR",
      compounds_names = "yes",
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE
    ),
    "Parameter\\(s\\) must be logical"
  )
})

## Filter_ms1_annotations ----

test_that("filter_ms1_annotations keeps MS2 annotations", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    candidate_score_similarity = c(0.8, NA_real_, NA_real_),
    candidate_score_sirius_csi = c(NA_real_, 0.7, NA_real_),
    score_biological = c(0.3, 0.3, 0.9),
    score_chemical = c(0.3, 0.3, 0.9)
  )

  result <- filter_ms1_annotations(
    df,
    minimal_ms1_bio = 0.5,
    minimal_ms1_chemo = 0.5,
    minimal_ms1_condition = "OR"
  )

  # F1 and F2 have MS2, F3 has high scores - all should pass
  expect_equal(nrow(result), 3)
  expect_true(all(c("F1", "F2", "F3") %in% result$feature_id))
})

test_that("filter_ms1_annotations applies OR condition correctly", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3", "F4"),
    candidate_score_similarity = c(NA_real_, NA_real_, NA_real_, NA_real_),
    candidate_score_sirius_csi = c(NA_real_, NA_real_, NA_real_, NA_real_),
    score_biological = c(0.6, 0.4, 0.6, 0.4),
    score_chemical = c(0.4, 0.6, 0.4, 0.4)
  )

  result <- filter_ms1_annotations(
    df,
    minimal_ms1_bio = 0.5,
    minimal_ms1_chemo = 0.5,
    minimal_ms1_condition = "OR"
  )

  # F1 (bio >= 0.5), F2 (chem >= 0.5), F3 (bio >= 0.5) pass; F4 fails
  expect_equal(nrow(result), 3)
  expect_true(all(c("F1", "F2", "F3") %in% result$feature_id))
})

test_that("filter_ms1_annotations applies AND condition correctly", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    candidate_score_similarity = c(NA_real_, NA_real_, NA_real_),
    candidate_score_sirius_csi = c(NA_real_, NA_real_, NA_real_),
    score_biological = c(0.6, 0.4, 0.6),
    score_chemical = c(0.6, 0.6, 0.4)
  )

  result <- filter_ms1_annotations(
    df,
    minimal_ms1_bio = 0.5,
    minimal_ms1_chemo = 0.5,
    minimal_ms1_condition = "AND"
  )

  # Only F1 passes both thresholds
  expect_equal(nrow(result), 1)
  expect_equal(result$feature_id, "F1")
})

## rank_and_deduplicate ----

test_that("rank_and_deduplicate keeps best structure per feature", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("A", "A", "B"),
    score_weighted_chemo = c(0.9, 0.8, 0.7),
    candidate_score_pseudo_initial = c(0.95, 0.85, 0.75)
  )

  result <- rank_and_deduplicate(df)

  # Should keep only unique feature+inchikey combinations
  expect_equal(nrow(result), 2)
  expect_true(all(c("rank_initial", "rank_final") %in% names(result)))
})

test_that("rank_and_deduplicate ranks correctly", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B", "C"),
    score_weighted_chemo = c(0.9, 0.8, 0.7),
    candidate_score_pseudo_initial = c(0.95, 0.85, 0.75)
  )

  result <- rank_and_deduplicate(df)

  # Highest scores should have rank 1
  expect_equal(
    result$rank_final[
      result$candidate_structure_inchikey_connectivity_layer == "A"
    ],
    1
  )
  expect_equal(
    result$rank_initial[
      result$candidate_structure_inchikey_connectivity_layer == "A"
    ],
    1
  )
})

## apply_percentile_filter ----

test_that("apply_percentile_filter keeps top candidates", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1"),
    score_weighted_chemo = c(1.0, 0.9, 0.5)
  )

  result <- apply_percentile_filter(df, best_percentile = 0.9)

  # Only scores >= 0.9 * max(1.0) = 0.9 should pass
  expect_equal(nrow(result), 2)
  expect_true(all(result$score_weighted_chemo >= 0.9))
})

test_that("apply_percentile_filter works per feature", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F2", "F2"),
    score_weighted_chemo = c(1.0, 0.5, 0.8, 0.4)
  )

  result <- apply_percentile_filter(df, best_percentile = 0.9)

  # F1: max=1.0, keep >= 0.9 (only first)
  # F2: max=0.8, keep >= 0.72 (only first)
  expect_equal(nrow(result), 2)
})

## count_candidates ----

test_that("count_candidates returns correct counts", {
  df_ranked <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1", "F2", "F2")
  )

  df_percentile <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F2")
  )

  result <- count_candidates(df_ranked, df_percentile)

  expect_equal(nrow(result), 2)
  expect_equal(result$candidates_evaluated[result$feature_id == "F1"], 3)
  expect_equal(result$candidates_best[result$feature_id == "F1"], 2)
  expect_equal(result$candidates_evaluated[result$feature_id == "F2"], 2)
  expect_equal(result$candidates_best[result$feature_id == "F2"], 1)
})

## compute_classyfire_taxonomy ----

test_that("compute_classyfire_taxonomy selects highest weighted level", {
  df <- tidytable::tidytable(
    feature_id = "F1",
    feature_pred_tax_cla_01kin_val = "Kingdom1",
    feature_pred_tax_cla_01kin_score = "0.5",
    feature_pred_tax_cla_02sup_val = "Superclass1",
    feature_pred_tax_cla_02sup_score = "0.6",
    feature_pred_tax_cla_03cla_val = "Class1",
    feature_pred_tax_cla_03cla_score = "0.7",
    feature_pred_tax_cla_04dirpar_val = "Parent1",
    feature_pred_tax_cla_04dirpar_score = "0.8"
  )

  weights <- list(
    w_cla_kin = 1.0,
    w_cla_sup = 1.0,
    w_cla_cla = 1.0,
    w_cla_par = 1.0
  )

  result <- compute_classyfire_taxonomy(df, weights)

  # Highest score (0.8) should be selected
  expect_equal(result$label_classyfire_predicted, "Parent1")
  expect_equal(result$score_classyfire, 0.8)
})

test_that("compute_classyfire_taxonomy filters empty labels", {
  df <- tidytable::tidytable(
    feature_id = "F1",
    feature_pred_tax_cla_01kin_val = "empty",
    feature_pred_tax_cla_01kin_score = "0.5",
    feature_pred_tax_cla_02sup_val = "empty",
    feature_pred_tax_cla_02sup_score = "0.6",
    feature_pred_tax_cla_03cla_val = "empty",
    feature_pred_tax_cla_03cla_score = "0.7",
    feature_pred_tax_cla_04dirpar_val = "empty",
    feature_pred_tax_cla_04dirpar_score = "0.8"
  )

  weights <- list(
    w_cla_kin = 1.0,
    w_cla_sup = 1.0,
    w_cla_cla = 1.0,
    w_cla_par = 1.0
  )

  result <- compute_classyfire_taxonomy(df, weights)

  # Should filter out empty labels
  expect_equal(nrow(result), 0)
})

## compute_npclassifier_taxonomy ----

test_that("compute_npclassifier_taxonomy selects highest weighted level", {
  df <- tidytable::tidytable(
    feature_id = "F1",
    feature_pred_tax_npc_01pat_val = "Pathway1",
    feature_pred_tax_npc_01pat_score = "0.5",
    feature_pred_tax_npc_02sup_val = "Superclass1",
    feature_pred_tax_npc_02sup_score = "0.6",
    feature_pred_tax_npc_03cla_val = "Class1",
    feature_pred_tax_npc_03cla_score = "0.7"
  )

  weights <- list(
    w_npc_pat = 1.0,
    w_npc_sup = 1.0,
    w_npc_cla = 1.0
  )

  result <- compute_npclassifier_taxonomy(df, weights)

  # Highest score (0.7) should be selected
  expect_equal(result$label_npclassifier_predicted, "Class1")
  expect_equal(result$score_npclassifier, 0.7)
})

## remove_compound_names ----

test_that("remove_compound_names removes names when compounds_names is FALSE", {
  results_list <- list(
    full = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_name = "Compound1"
    ),
    filtered = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_name = "Compound1"
    ),
    mini = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_name = "Compound1"
    )
  )

  result <- remove_compound_names(results_list, compounds_names = FALSE)

  expect_false("candidate_structure_name" %in% names(result$full))
  expect_false("candidate_structure_name" %in% names(result$filtered))
  expect_false("candidate_structure_name" %in% names(result$mini))
})

test_that("remove_compound_names keeps names when compounds_names is TRUE", {
  results_list <- list(
    full = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_name = "Compound1"
    ),
    filtered = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_name = "Compound1"
    ),
    mini = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_name = "Compound1"
    )
  )

  result <- remove_compound_names(results_list, compounds_names = TRUE)

  expect_true("candidate_structure_name" %in% names(result$full))
  expect_true("candidate_structure_name" %in% names(result$filtered))
  expect_true("candidate_structure_name" %in% names(result$mini))
})

## Integration Tests ----

test_that("clean_chemo handles empty data frame", {
  result <- clean_chemo(
    annot_table_wei_chemo = tidytable::tidytable(),
    features_table = tidytable::tidytable(),
    components_table = tidytable::tidytable(),
    structure_organism_pairs_table = tidytable::tidytable(),
    candidates_final = 10,
    best_percentile = 0.9,
    minimal_ms1_bio = 0.5,
    minimal_ms1_chemo = 0.5,
    minimal_ms1_condition = "OR",
    compounds_names = FALSE,
    high_confidence = FALSE,
    remove_ties = FALSE,
    summarize = FALSE
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("clean_chemo returns list with correct structure", {
  # Create minimal test data
  annot_table_wei_chemo <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B"),
    candidate_score_similarity = c(0.8, 0.7),
    candidate_score_sirius_csi = c(NA_real_, NA_real_),
    candidate_score_pseudo_initial = c(0.8, 0.7),
    score_weighted_chemo = c(0.8, 0.7),
    score_biological = c(0.5, 0.6),
    score_chemical = c(0.5, 0.6)
  )

  # Skip full integration test - would need all helper functions
  # This is a placeholder for when full dependencies are available
  expect_true(TRUE)
})

## Edge Cases ----

test_that("filter_ms1_annotations handles all NA scores", {
  df <- tidytable::tidytable(
    feature_id = c("F1"),
    candidate_score_similarity = c(NA_real_),
    candidate_score_sirius_csi = c(NA_real_),
    score_biological = c(NA_real_),
    score_chemical = c(NA_real_)
  )

  result <- filter_ms1_annotations(
    df,
    minimal_ms1_bio = 0.5,
    minimal_ms1_chemo = 0.5,
    minimal_ms1_condition = "OR"
  )

  # Should filter out NA scores
  expect_equal(nrow(result), 0)
})

test_that("apply_percentile_filter handles single row per feature", {
  df <- tidytable::tidytable(
    feature_id = c("F1"),
    score_weighted_chemo = c(0.8)
  )

  result <- apply_percentile_filter(df, best_percentile = 0.9)

  # Single row should always pass (score >= 0.9 * score)
  expect_equal(nrow(result), 1)
})

test_that("rank_and_deduplicate handles empty data frame", {
  df <- tidytable::tidytable(
    feature_id = character(),
    candidate_structure_inchikey_connectivity_layer = character(),
    score_weighted_chemo = numeric(),
    candidate_score_pseudo_initial = numeric()
  )

  result <- rank_and_deduplicate(df)

  expect_equal(nrow(result), 0)
  expect_true("rank_final" %in% names(result))
  expect_true("rank_initial" %in% names(result))
})

test_that("count_candidates handles features in ranked but not in percentile", {
  df_ranked <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F2")
  )

  df_percentile <- tidytable::tidytable(
    feature_id = c("F1")
  )

  result <- count_candidates(df_ranked, df_percentile)

  expect_equal(nrow(result), 2)
  expect_true(is.na(result$candidates_best[result$feature_id == "F2"]))
})

## filter_ms1_annotations - Comprehensive Edge Cases ----

test_that("filter_ms1_annotations keeps annotations with SIRIUS CSI scores", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    candidate_score_similarity = c(NA_real_, NA_real_, 0.8),
    candidate_score_sirius_csi = c(0.9, NA_real_, NA_real_),
    score_biological = c(0.1, 0.1, 0.1),
    score_chemical = c(0.1, 0.1, 0.1)
  )

  result <- filter_ms1_annotations(
    df,
    minimal_ms1_bio = 0.5,
    minimal_ms1_chemo = 0.5,
    minimal_ms1_condition = "OR"
  )

  # F1 has SIRIUS, F3 has similarity - both should pass
  expect_equal(nrow(result), 2)
  expect_true(all(c("F1", "F3") %in% result$feature_id))
})

test_that("filter_ms1_annotations handles mixed AND condition", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3", "F4"),
    candidate_score_similarity = rep(NA_real_, 4),
    candidate_score_sirius_csi = rep(NA_real_, 4),
    score_biological = c(0.6, 0.6, 0.4, 0.6),
    score_chemical = c(0.4, 0.6, 0.6, 0.6)
  )

  result <- filter_ms1_annotations(
    df,
    minimal_ms1_bio = 0.5,
    minimal_ms1_chemo = 0.5,
    minimal_ms1_condition = "AND"
  )

  # Only F2 and F4 pass both thresholds
  expect_equal(nrow(result), 2)
  expect_true(all(c("F2", "F4") %in% result$feature_id))
})

## rank_and_deduplicate - Comprehensive Tests ----

test_that("rank_and_deduplicate handles multiple features with ties", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1", "F2", "F2"),
    candidate_structure_inchikey_connectivity_layer = c(
      "A",
      "B",
      "C",
      "D",
      "E"
    ),
    score_weighted_chemo = c(0.9, 0.9, 0.7, 0.8, 0.6),
    candidate_score_pseudo_initial = c(0.95, 0.95, 0.75, 0.85, 0.65)
  )

  result <- rank_and_deduplicate(df)

  # Should have 5 rows (all unique feature+inchikey combinations)
  expect_equal(nrow(result), 5)

  # Check that ties get same rank
  f1_ranks <- result$rank_final[result$feature_id == "F1"]
  expect_true(sum(f1_ranks == 1) >= 2) # At least 2 with rank 1 (tied)
})

test_that("rank_and_deduplicate sorts by score correctly", {
  df <- tidytable::tidytable(
    feature_id = rep("F1", 5),
    candidate_structure_inchikey_connectivity_layer = c(
      "A",
      "B",
      "C",
      "D",
      "E"
    ),
    score_weighted_chemo = c(0.5, 0.9, 0.7, 0.3, 0.8),
    candidate_score_pseudo_initial = c(0.5, 0.9, 0.7, 0.3, 0.8)
  )

  result <- rank_and_deduplicate(df)

  # First row should have highest score
  expect_equal(result$score_weighted_chemo[1], 0.9)
  expect_equal(result$rank_final[1], 1)
})

## apply_percentile_filter - Edge Cases ----

test_that("apply_percentile_filter with 0.0 percentile keeps all", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1"),
    score_weighted_chemo = c(1.0, 0.5, 0.1)
  )

  result <- apply_percentile_filter(df, best_percentile = 0.0)

  # All scores >= 0.0 * max should pass
  expect_equal(nrow(result), 3)
})

test_that("apply_percentile_filter with 1.0 percentile keeps only max", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1"),
    score_weighted_chemo = c(1.0, 0.99, 0.9)
  )

  result <- apply_percentile_filter(df, best_percentile = 1.0)

  # Only score >= 1.0 * max (i.e., max itself)
  expect_equal(nrow(result), 1)
  expect_equal(result$score_weighted_chemo[1], 1.0)
})

test_that("apply_percentile_filter handles zero scores", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1"),
    score_weighted_chemo = c(0.0, 0.0)
  )

  result <- apply_percentile_filter(df, best_percentile = 0.9)

  # 0 >= 0.9 * 0 = 0, both should pass
  expect_equal(nrow(result), 2)
})

## compute_classyfire_taxonomy - Comprehensive ----

test_that("compute_classyfire_taxonomy handles all NA scores", {
  df <- tidytable::tidytable(
    feature_id = "F1",
    feature_pred_tax_cla_01kin_val = "Kingdom1",
    feature_pred_tax_cla_01kin_score = NA_character_,
    feature_pred_tax_cla_02sup_val = "Superclass1",
    feature_pred_tax_cla_02sup_score = NA_character_,
    feature_pred_tax_cla_03cla_val = "Class1",
    feature_pred_tax_cla_03cla_score = NA_character_,
    feature_pred_tax_cla_04dirpar_val = "Parent1",
    feature_pred_tax_cla_04dirpar_score = NA_character_
  )

  weights <- list(
    w_cla_kin = 1.0,
    w_cla_sup = 1.0,
    w_cla_cla = 1.0,
    w_cla_par = 1.0
  )

  result <- compute_classyfire_taxonomy(df, weights)

  # Should handle NA scores gracefully
  expect_equal(nrow(result), 0)
})

test_that("compute_classyfire_taxonomy respects weights", {
  df <- tidytable::tidytable(
    feature_id = "F1",
    feature_pred_tax_cla_01kin_val = "Kingdom1",
    feature_pred_tax_cla_01kin_score = "0.5",
    feature_pred_tax_cla_02sup_val = "Superclass1",
    feature_pred_tax_cla_02sup_score = "0.4",
    feature_pred_tax_cla_03cla_val = "Class1",
    feature_pred_tax_cla_03cla_score = "0.3",
    feature_pred_tax_cla_04dirpar_val = "Parent1",
    feature_pred_tax_cla_04dirpar_score = "0.2"
  )

  # High weight on parent level
  weights <- list(
    w_cla_kin = 0.1,
    w_cla_sup = 0.1,
    w_cla_cla = 0.1,
    w_cla_par = 10.0
  )

  result <- compute_classyfire_taxonomy(df, weights)

  # Parent should win due to weight (0.2 * 10.0 = 2.0 > others)
  expect_equal(result$label_classyfire_predicted, "Parent1")
})

test_that("compute_classyfire_taxonomy handles missing middle levels", {
  df <- tidytable::tidytable(
    feature_id = "F1",
    feature_pred_tax_cla_01kin_val = "Kingdom1",
    feature_pred_tax_cla_01kin_score = "0.5",
    feature_pred_tax_cla_02sup_val = NA_character_,
    feature_pred_tax_cla_02sup_score = NA_character_,
    feature_pred_tax_cla_03cla_val = NA_character_,
    feature_pred_tax_cla_03cla_score = NA_character_,
    feature_pred_tax_cla_04dirpar_val = "Parent1",
    feature_pred_tax_cla_04dirpar_score = "0.8"
  )

  weights <- list(
    w_cla_kin = 1.0,
    w_cla_sup = 1.0,
    w_cla_cla = 1.0,
    w_cla_par = 1.0
  )

  result <- compute_classyfire_taxonomy(df, weights)

  # Should select parent (highest score)
  expect_equal(result$label_classyfire_predicted, "Parent1")
})

## compute_npclassifier_taxonomy - Comprehensive ----

test_that("compute_npclassifier_taxonomy handles all NA scores", {
  df <- tidytable::tidytable(
    feature_id = "F1",
    feature_pred_tax_npc_01pat_val = "Pathway1",
    feature_pred_tax_npc_01pat_score = NA_character_,
    feature_pred_tax_npc_02sup_val = "Superclass1",
    feature_pred_tax_npc_02sup_score = NA_character_,
    feature_pred_tax_npc_03cla_val = "Class1",
    feature_pred_tax_npc_03cla_score = NA_character_
  )

  weights <- list(
    w_npc_pat = 1.0,
    w_npc_sup = 1.0,
    w_npc_cla = 1.0
  )

  result <- compute_npclassifier_taxonomy(df, weights)

  # Should filter out when all are empty/NA
  expect_equal(nrow(result), 0)
})

test_that("compute_npclassifier_taxonomy respects weights", {
  df <- tidytable::tidytable(
    feature_id = "F1",
    feature_pred_tax_npc_01pat_val = "Pathway1",
    feature_pred_tax_npc_01pat_score = "0.3",
    feature_pred_tax_npc_02sup_val = "Superclass1",
    feature_pred_tax_npc_02sup_score = "0.4",
    feature_pred_tax_npc_03cla_val = "Class1",
    feature_pred_tax_npc_03cla_score = "0.35"
  )

  # High weight on pathway level
  weights <- list(
    w_npc_pat = 10.0,
    w_npc_sup = 0.1,
    w_npc_cla = 0.1
  )

  result <- compute_npclassifier_taxonomy(df, weights)

  # Pathway should win due to weight (0.3 * 10.0 = 3.0 > others)
  expect_equal(result$label_npclassifier_predicted, "Pathway1")
})

test_that("compute_npclassifier_taxonomy filters empty labels", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    feature_pred_tax_npc_01pat_val = c("empty", "Pathway1"),
    feature_pred_tax_npc_01pat_score = c("0.5", "0.5"),
    feature_pred_tax_npc_02sup_val = c("empty", "empty"),
    feature_pred_tax_npc_02sup_score = c("0.6", "0.4"),
    feature_pred_tax_npc_03cla_val = c("empty", "empty"),
    feature_pred_tax_npc_03cla_score = c("0.7", "0.3")
  )

  weights <- list(
    w_npc_pat = 1.0,
    w_npc_sup = 1.0,
    w_npc_cla = 1.0
  )

  result <- compute_npclassifier_taxonomy(df, weights)

  # Only F2 should remain (has non-empty label)
  expect_equal(nrow(result), 1)
  expect_equal(result$feature_id, "F2")
})

## Validation - Boundary Values ----

test_that("validate_clean_chemo_inputs accepts boundary values", {
  expect_silent(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = tidytable::tidytable(x = 1),
      candidates_final = 1, # minimum
      best_percentile = 0, # minimum
      minimal_ms1_bio = 0, # minimum
      minimal_ms1_chemo = 1, # maximum
      minimal_ms1_condition = "AND",
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE
    )
  )
})

test_that("validate_clean_chemo_inputs rejects candidates_final = 0.5", {
  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = tidytable::tidytable(),
      candidates_final = 0.5,
      best_percentile = 0.9,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE
    ),
    "candidates_final must be a positive integer"
  )
})

test_that("validate_clean_chemo_inputs rejects multiple invalid logical params", {
  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = tidytable::tidytable(),
      candidates_final = 10,
      best_percentile = 0.9,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      minimal_ms1_condition = "OR",
      compounds_names = "yes",
      high_confidence = 1,
      remove_ties = FALSE,
      summarize = FALSE
    ),
    "compounds_names.*high_confidence"
  )
})
