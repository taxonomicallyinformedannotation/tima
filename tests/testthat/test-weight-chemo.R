# Test: Weight Chemo - Chemical Consistency Scoring
library(testthat)

# =============================================================================
# Tests for weight_chemo() - Chemical Consistency Weighting
# =============================================================================

test_that("weight_chemo validates input data frame", {
  expect_error(
    weight_chemo(
      annot_table_wei_bio_clean = "not a dataframe",
      weight_spectral = 0.33,
      weight_biological = 0.33,
      weight_chemical = 0.34,
      score_chemical_cla_kingdom = 0.1,
      score_chemical_cla_superclass = 0.3,
      score_chemical_cla_class = 0.5,
      score_chemical_cla_parent = 1.0,
      score_chemical_npc_pathway = 0.2,
      score_chemical_npc_superclass = 0.5,
      score_chemical_npc_class = 1.0
    ),
    "must be a data frame"
  )
})

test_that("weight_chemo handles empty input gracefully", {
  copy_backbone(cache_dir = ".")

  empty_table <- tidytable::tidytable(
    feature_id = character(0)
  )

  result <- weight_chemo(
    annot_table_wei_bio_clean = empty_table,
    weight_spectral = 0.33,
    weight_biological = 0.33,
    weight_chemical = 0.34,
    score_chemical_cla_kingdom = 0.1,
    score_chemical_cla_superclass = 0.3,
    score_chemical_cla_class = 0.5,
    score_chemical_cla_parent = 1.0,
    score_chemical_npc_pathway = 0.2,
    score_chemical_npc_superclass = 0.5,
    score_chemical_npc_class = 1.0
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)

  unlink("data", recursive = TRUE)
})

test_that("weight_chemo validates weights sum to 1", {
  copy_backbone(cache_dir = ".")

  test_table <- tidytable::tidytable(
    feature_id = c("FT001"),
    candidate_structure_inchikey = c("AAAAA-BBBBB-C")
  )

  # Test weights that don't sum to 1
  expect_error(
    weight_chemo(
      annot_table_wei_bio_clean = test_table,
      weight_spectral = 0.5,
      weight_biological = 0.3,
      weight_chemical = 0.1, # Sum = 0.9
      score_chemical_cla_kingdom = 0.1,
      score_chemical_cla_superclass = 0.3,
      score_chemical_cla_class = 0.5,
      score_chemical_cla_parent = 1.0,
      score_chemical_npc_pathway = 0.2,
      score_chemical_npc_superclass = 0.5,
      score_chemical_npc_class = 1.0
    ),
    "must sum to 1"
  )

  unlink("data", recursive = TRUE)
})

test_that("weight_chemo validates weights are non-negative", {
  copy_backbone(cache_dir = ".")

  test_table <- tidytable::tidytable(
    feature_id = c("FT001"),
    candidate_structure_inchikey = c("AAAAA-BBBBB-C")
  )

  # Test negative weight
  expect_error(
    weight_chemo(
      annot_table_wei_bio_clean = test_table,
      weight_spectral = 0.5,
      weight_biological = -0.2, # Negative
      weight_chemical = 0.7,
      score_chemical_cla_kingdom = 0.1,
      score_chemical_cla_superclass = 0.3,
      score_chemical_cla_class = 0.5,
      score_chemical_cla_parent = 1.0,
      score_chemical_npc_pathway = 0.2,
      score_chemical_npc_superclass = 0.5,
      score_chemical_npc_class = 1.0
    ),
    "non-negative"
  )

  unlink("data", recursive = TRUE)
})

test_that("weight_chemo validates ClassyFire scores", {
  copy_backbone(cache_dir = ".")

  test_table <- tidytable::tidytable(
    feature_id = c("FT001"),
    candidate_structure_inchikey = c("AAAAA-BBBBB-C")
  )

  # Test invalid kingdom score
  expect_error(
    weight_chemo(
      annot_table_wei_bio_clean = test_table,
      weight_spectral = 0.33,
      weight_biological = 0.33,
      weight_chemical = 0.34,
      score_chemical_cla_kingdom = 1.5, # > 1
      score_chemical_cla_superclass = 0.3,
      score_chemical_cla_class = 0.5,
      score_chemical_cla_parent = 1.0,
      score_chemical_npc_pathway = 0.2,
      score_chemical_npc_superclass = 0.5,
      score_chemical_npc_class = 1.0
    ),
    "must be between 0 and 1"
  )

  # Test invalid superclass score
  expect_error(
    weight_chemo(
      annot_table_wei_bio_clean = test_table,
      weight_spectral = 0.33,
      weight_biological = 0.33,
      weight_chemical = 0.34,
      score_chemical_cla_kingdom = 0.1,
      score_chemical_cla_superclass = -0.1, # < 0
      score_chemical_cla_class = 0.5,
      score_chemical_cla_parent = 1.0,
      score_chemical_npc_pathway = 0.2,
      score_chemical_npc_superclass = 0.5,
      score_chemical_npc_class = 1.0
    ),
    "must be between 0 and 1"
  )

  unlink("data", recursive = TRUE)
})

test_that("weight_chemo validates NPClassifier scores", {
  copy_backbone(cache_dir = ".")

  test_table <- tidytable::tidytable(
    feature_id = c("FT001"),
    candidate_structure_inchikey = c("AAAAA-BBBBB-C")
  )

  # Test invalid pathway score
  expect_error(
    weight_chemo(
      annot_table_wei_bio_clean = test_table,
      weight_spectral = 0.33,
      weight_biological = 0.33,
      weight_chemical = 0.34,
      score_chemical_cla_kingdom = 0.1,
      score_chemical_cla_superclass = 0.3,
      score_chemical_cla_class = 0.5,
      score_chemical_cla_parent = 1.0,
      score_chemical_npc_pathway = 2.0, # > 1
      score_chemical_npc_superclass = 0.5,
      score_chemical_npc_class = 1.0
    ),
    "must be between 0 and 1"
  )

  # Test invalid class score
  expect_error(
    weight_chemo(
      annot_table_wei_bio_clean = test_table,
      weight_spectral = 0.33,
      weight_biological = 0.33,
      weight_chemical = 0.34,
      score_chemical_cla_kingdom = 0.1,
      score_chemical_cla_superclass = 0.3,
      score_chemical_cla_class = 0.5,
      score_chemical_cla_parent = 1.0,
      score_chemical_npc_pathway = 0.2,
      score_chemical_npc_superclass = 0.5,
      score_chemical_npc_class = -0.5 # < 0
    ),
    "must be between 0 and 1"
  )

  unlink("data", recursive = TRUE)
})

test_that("weight_chemo validates all chemical scores vectorized", {
  copy_backbone(cache_dir = ".")

  test_table <- tidytable::tidytable(
    feature_id = c("FT001"),
    candidate_structure_inchikey = c("AAAAA-BBBBB-C")
  )

  # Multiple invalid scores should be caught
  expect_error(
    weight_chemo(
      annot_table_wei_bio_clean = test_table,
      weight_spectral = 0.33,
      weight_biological = 0.33,
      weight_chemical = 0.34,
      score_chemical_cla_kingdom = 1.5, # Invalid
      score_chemical_cla_superclass = -0.1, # Invalid
      score_chemical_cla_class = 0.5,
      score_chemical_cla_parent = 1.0,
      score_chemical_npc_pathway = 2.0, # Invalid
      score_chemical_npc_superclass = 0.5,
      score_chemical_npc_class = 1.0
    ),
    "must be between 0 and 1"
  )

  unlink("data", recursive = TRUE)
})

test_that("weight_chemo chemical score hierarchy makes sense", {
  # Verify that chemical taxonomy scores increase with specificity

  # ClassyFire hierarchy: kingdom < superclass < class < parent
  cla_scores <- list(
    kingdom = 0.1,
    superclass = 0.3,
    class = 0.5,
    parent = 1.0
  )

  cla_values <- unlist(cla_scores)
  expect_true(
    all(diff(cla_values) > 0),
    info = "ClassyFire scores should increase with specificity"
  )

  # NPClassifier hierarchy: pathway < superclass < class
  npc_scores <- list(
    pathway = 0.2,
    superclass = 0.5,
    class = 1.0
  )

  npc_values <- unlist(npc_scores)
  expect_true(
    all(diff(npc_values) > 0),
    info = "NPClassifier scores should increase with specificity"
  )
})

test_that("weight_chemo accepts valid parameters", {
  copy_backbone(cache_dir = ".")

  test_table <- tidytable::tidytable(
    feature_id = c("FT001", "FT002"),
    candidate_structure_inchikey = c("AAAAA-BBBBB-C", "DDDDD-EEEEE-F"),
    component_id = c("1", "1"),
    candidate_structure_tax_cla_chemontid = c(
      "CHEMONTID:0000001",
      "CHEMONTID:0000001"
    ),
    candidate_structure_tax_cla_01kin = c(
      "Organic compounds",
      "Organic compounds"
    ),
    candidate_structure_tax_cla_02sup = c("Alkaloids", "Alkaloids"),
    candidate_structure_tax_cla_03cla = c(
      "Indole alkaloids",
      "Indole alkaloids"
    ),
    candidate_structure_tax_cla_04dirpar = c("Indole parent", "Indole parent"),
    candidate_structure_tax_npc_01pat = c("Alkaloids", "Alkaloids"),
    candidate_structure_tax_npc_02sup = c(
      "Indole alkaloids",
      "Indole alkaloids"
    ),
    candidate_structure_tax_npc_03cla = c("Simple indoles", "Simple indoles"),
    feature_pred_tax_cla_01kin_val = c(
      "Organic compounds",
      "Organic compounds"
    ),
    feature_pred_tax_cla_01kin_score = c("0.95", "0.95"),
    feature_pred_tax_cla_02sup_val = c("Alkaloids", "Alkaloids"),
    feature_pred_tax_cla_02sup_score = c("0.90", "0.90"),
    feature_pred_tax_cla_03cla_val = c("Indole alkaloids", "Indole alkaloids"),
    feature_pred_tax_cla_03cla_score = c("0.85", "0.85"),
    feature_pred_tax_cla_04dirpar_val = c("Indole parent", "Indole parent"),
    feature_pred_tax_cla_04dirpar_score = c("0.80", "0.80"),
    feature_pred_tax_npc_01pat_val = c("Alkaloids", "Alkaloids"),
    feature_pred_tax_npc_01pat_score = c("0.90", "0.90"),
    feature_pred_tax_npc_02sup_val = c("Indole alkaloids", "Indole alkaloids"),
    feature_pred_tax_npc_02sup_score = c("0.85", "0.85"),
    feature_pred_tax_npc_03cla_val = c("Simple indoles", "Simple indoles"),
    feature_pred_tax_npc_03cla_score = c("0.80", "0.80"),
    candidate_score_sirius_csi = c("0.95", "0.85"),
    candidate_score_pseudo_initial = c(0.90, 0.80),
    score_biological = c(0.80, 0.70)
  )

  # Should complete without error
  expect_no_error({
    result <- weight_chemo(
      annot_table_wei_bio_clean = test_table,
      weight_spectral = 0.33,
      weight_biological = 0.33,
      weight_chemical = 0.34,
      score_chemical_cla_kingdom = 0.1,
      score_chemical_cla_superclass = 0.3,
      score_chemical_cla_class = 0.5,
      score_chemical_cla_parent = 1.0,
      score_chemical_npc_pathway = 0.2,
      score_chemical_npc_superclass = 0.5,
      score_chemical_npc_class = 1.0
    )
  })

  unlink("data", recursive = TRUE)
})
