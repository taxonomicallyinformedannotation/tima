# Test Suite: weight_chemo ----

library(testthat)

## Input Validation ----

test_that("weight_chemo validates data frame input", {
  # Use fixture utilities
  empty_table <- tidytable::tidytable(
    feature_id = character(0),
    candidate_structure_inchikey_connectivity_layer = character(0)
  )

  expect_error(
    weight_chemo(
      annot_table_wei_bio_clean = "not_a_dataframe",
      weight_spectral = 0.33,
      weight_biological = 0.33,
      weight_chemical = 0.34,
      score_chemical_cla_kingdom = 0.1,
      score_chemical_cla_superclass = 0.2,
      score_chemical_cla_class = 0.3,
      score_chemical_cla_parent = 0.4,
      score_chemical_npc_pathway = 0.5,
      score_chemical_npc_superclass = 0.6,
      score_chemical_npc_class = 0.7
    ),
    "data frame"
  )
})

test_that("weight_chemo handles empty data frame", {
  # Use fixture utilities
  empty_annot <- tidytable::tidytable(
    feature_id = character(0),
    candidate_structure_inchikey_connectivity_layer = character(0)
  )

  result <- weight_chemo(
    annot_table_wei_bio_clean = empty_annot,
    weight_spectral = 0.33,
    weight_biological = 0.33,
    weight_chemical = 0.34,
    score_chemical_cla_kingdom = 0.1,
    score_chemical_cla_superclass = 0.2,
    score_chemical_cla_class = 0.3,
    score_chemical_cla_parent = 0.4,
    score_chemical_npc_pathway = 0.5,
    score_chemical_npc_superclass = 0.6,
    score_chemical_npc_class = 0.7
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("weight_chemo validates weight ranges and sum", {
  # Create minimal non-empty annotation table
  annot_table <- tidytable::tidytable(
    feature_id = "F1",
    candidate_structure_inchikey_connectivity_layer = "ABCDEFGHIJKLMN",
    score_biological = 0.5,
    candidate_score_pseudo_initial = 0
  )

  # Negative weight
  expect_error(
    weight_chemo(
      annot_table_wei_bio_clean = annot_table,
      weight_spectral = -0.1,
      weight_biological = 0.5,
      weight_chemical = 0.6,
      score_chemical_cla_kingdom = 0.1,
      score_chemical_cla_superclass = 0.2,
      score_chemical_cla_class = 0.3,
      score_chemical_cla_parent = 0.4,
      score_chemical_npc_pathway = 0.5,
      score_chemical_npc_superclass = 0.6,
      score_chemical_npc_class = 0.7
    ),
    "non-negative"
  )

  # Weights don't sum to 1
  expect_error(
    weight_chemo(
      annot_table_wei_bio_clean = annot_table,
      weight_spectral = 0.5,
      weight_biological = 0.5,
      weight_chemical = 0.5,
      score_chemical_cla_kingdom = 0.1,
      score_chemical_cla_superclass = 0.2,
      score_chemical_cla_class = 0.3,
      score_chemical_cla_parent = 0.4,
      score_chemical_npc_pathway = 0.5,
      score_chemical_npc_superclass = 0.6,
      score_chemical_npc_class = 0.7
    ),
    "sum to 1"
  )
})

test_that("weight_chemo validates score ranges", {
  # Create minimal non-empty annotation table
  annot_table <- tidytable::tidytable(
    feature_id = "F1",
    candidate_structure_inchikey_connectivity_layer = "ABCDEFGHIJKLMN",
    score_biological = 0.5,
    candidate_score_pseudo_initial = 0
  )

  # Negative score
  expect_error(
    weight_chemo(
      annot_table_wei_bio_clean = annot_table,
      weight_spectral = 0.33,
      weight_biological = 0.33,
      weight_chemical = 0.34,
      score_chemical_cla_kingdom = -0.1,
      score_chemical_cla_superclass = 0.2,
      score_chemical_cla_class = 0.3,
      score_chemical_cla_parent = 0.4,
      score_chemical_npc_pathway = 0.5,
      score_chemical_npc_superclass = 0.6,
      score_chemical_npc_class = 0.7
    ),
    "between 0 and 1"
  )

  # Score > 1
  expect_error(
    weight_chemo(
      annot_table_wei_bio_clean = annot_table,
      weight_spectral = 0.33,
      weight_biological = 0.33,
      weight_chemical = 0.34,
      score_chemical_cla_kingdom = 0.1,
      score_chemical_cla_superclass = 1.5,
      score_chemical_cla_class = 0.3,
      score_chemical_cla_parent = 0.4,
      score_chemical_npc_pathway = 0.5,
      score_chemical_npc_superclass = 0.6,
      score_chemical_npc_class = 0.7
    ),
    "between 0 and 1"
  )
})

## Functional scoring ----

test_that("weight_chemo assigns chemical scores from taxonomy matches", {
  # One row where candidate class matches predicted class; others NA
  annot_table <- tidytable::tidytable(
    feature_id = "F1",
    candidate_structure_inchikey_connectivity_layer = "ABCDEFGHIJKLMN",
    # candidate taxonomy
    candidate_structure_tax_cla_01kin = NA_character_,
    candidate_structure_tax_cla_02sup = NA_character_,
    candidate_structure_tax_cla_03cla = "Flavonols",
    candidate_structure_tax_cla_04dirpar = NA_character_,
    candidate_structure_tax_npc_01pat = NA_character_,
    candidate_structure_tax_npc_02sup = NA_character_,
    candidate_structure_tax_npc_03cla = NA_character_,
    # feature predicted taxonomy values
    feature_pred_tax_cla_01kin_val = NA_character_,
    feature_pred_tax_cla_01kin_score = NA_character_,
    feature_pred_tax_cla_02sup_val = NA_character_,
    feature_pred_tax_cla_02sup_score = NA_character_,
    feature_pred_tax_cla_03cla_val = "Flavonols",
    feature_pred_tax_cla_03cla_score = "0.9",
    feature_pred_tax_cla_04dirpar_val = NA_character_,
    feature_pred_tax_cla_04dirpar_score = NA_character_,
    feature_pred_tax_npc_01pat_val = NA_character_,
    feature_pred_tax_npc_01pat_score = NA_character_,
    feature_pred_tax_npc_02sup_val = NA_character_,
    feature_pred_tax_npc_02sup_score = NA_character_,
    feature_pred_tax_npc_03cla_val = NA_character_,
    feature_pred_tax_npc_03cla_score = NA_character_,
    # other required fields
    candidate_score_pseudo_initial = 0,
    score_biological = 0
  )

  res <- weight_chemo(
    annot_table_wei_bio_clean = annot_table,
    weight_spectral = 0,
    weight_biological = 0,
    weight_chemical = 1,
    score_chemical_cla_kingdom = 0.1,
    score_chemical_cla_superclass = 0.2,
    score_chemical_cla_class = 0.7,
    score_chemical_cla_parent = 0.9,
    score_chemical_npc_pathway = 0.15,
    score_chemical_npc_superclass = 0.25,
    score_chemical_npc_class = 0.35
  )

  expect_s3_class(res, "data.frame")
  # chemical score should come from class level match (0.7)
  expect_true("score_chemical" %in% names(res))
  expect_equal(res$score_chemical, 0.7)
  # weighted chemo equals chemical when other weights are zero
  expect_equal(res$score_weighted_chemo, 0.7)
})

# Combined Scoring ----

test_that("weight_chemo combines ClassyFire and NPClassifier scores", {
  skip_on_cran()
  skip("Not implemented")

  # This test would verify correct combination of multiple taxonomy sources
})
