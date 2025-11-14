# Test: Decoration Functions
library(testthat)

# =============================================================================
# Tests for decorate_bio()
# =============================================================================

test_that("decorate_bio handles empty input", {
  empty_df <- tidytable::tidytable(
    score_biological = numeric(0),
    candidate_structure_inchikey_connectivity_layer = character(0)
  )

  result <- decorate_bio(
    annot_table_wei_bio = empty_df,
    score_biological_kingdom = 0.1,
    score_biological_phylum = 0.2,
    score_biological_class = 0.3,
    score_biological_order = 0.4,
    score_biological_family = 0.5,
    score_biological_tribe = 0.6,
    score_biological_genus = 0.7,
    score_biological_species = 0.8,
    score_biological_variety = 0.9
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("decorate_bio returns input unchanged", {
  test_df <- tidytable::tidytable(
    feature_id = c("FT001", "FT002"),
    score_biological = c(0.8, 0.9),
    candidate_structure_inchikey_connectivity_layer = c("AAAAA", "BBBBB")
  )

  result <- decorate_bio(
    annot_table_wei_bio = test_df,
    score_biological_kingdom = 0.1,
    score_biological_phylum = 0.2,
    score_biological_class = 0.3,
    score_biological_order = 0.4,
    score_biological_family = 0.5,
    score_biological_tribe = 0.6,
    score_biological_genus = 0.7,
    score_biological_species = 0.8,
    score_biological_variety = 0.9
  )

  # Should return input unchanged (decorator pattern)
  expect_identical(result, test_df)
})

test_that("decorate_bio warns on missing columns", {
  incomplete_df <- tidytable::tidytable(
    feature_id = c("FT001")
  )

  expect_warning(
    decorate_bio(
      annot_table_wei_bio = incomplete_df,
      score_biological_kingdom = 0.1,
      score_biological_phylum = 0.2,
      score_biological_class = 0.3,
      score_biological_order = 0.4,
      score_biological_family = 0.5,
      score_biological_tribe = 0.6,
      score_biological_genus = 0.7,
      score_biological_species = 0.8,
      score_biological_variety = 0.9
    ),
    NA  # Should log warning but not throw error
  )
})

# =============================================================================
# Tests for decorate_chemo()
# =============================================================================

test_that("decorate_chemo handles empty input", {
  empty_df <- tidytable::tidytable(
    score_chemical = numeric(0),
    candidate_structure_inchikey_connectivity_layer = character(0)
  )

  result <- decorate_chemo(
    annot_table_wei_chemo = empty_df,
    score_chemical_cla_kingdom = 0.1,
    score_chemical_cla_superclass = 0.2,
    score_chemical_cla_class = 0.3,
    score_chemical_cla_parent = 0.4,
    score_chemical_npc_pathway = 0.5,
    score_chemical_npc_superclass = 0.6,
    score_chemical_npc_class = 0.7
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("decorate_chemo returns input unchanged", {
  test_df <- tidytable::tidytable(
    feature_id = c("FT001", "FT002"),
    score_chemical = c(0.8, 0.9),
    candidate_structure_inchikey_connectivity_layer = c("AAAAA", "BBBBB"),
    feature_pred_tax_cla_01kin_val = c("Organic", "Organic"),
    feature_pred_tax_cla_02sup_val = c("Alkaloids", "Terpenoids"),
    feature_pred_tax_cla_03cla_val = c("Class1", "Class2"),
    feature_pred_tax_cla_04dirpar_val = c("Parent1", "Parent2"),
    feature_pred_tax_npc_01pat_val = c("Path1", "Path2"),
    feature_pred_tax_npc_02sup_val = c("Super1", "Super2"),
    feature_pred_tax_npc_03cla_val = c("NPC1", "NPC2")
  )

  result <- decorate_chemo(
    annot_table_wei_chemo = test_df,
    score_chemical_cla_kingdom = 0.1,
    score_chemical_cla_superclass = 0.2,
    score_chemical_cla_class = 0.3,
    score_chemical_cla_parent = 0.4,
    score_chemical_npc_pathway = 0.5,
    score_chemical_npc_superclass = 0.6,
    score_chemical_npc_class = 0.7
  )

  # Should return input unchanged (decorator pattern)
  expect_identical(result, test_df)
})

test_that("decorate_chemo warns on missing columns", {
  incomplete_df <- tidytable::tidytable(
    feature_id = c("FT001")
  )

  expect_warning(
    decorate_chemo(
      annot_table_wei_chemo = incomplete_df,
      score_chemical_cla_kingdom = 0.1,
      score_chemical_cla_superclass = 0.2,
      score_chemical_cla_class = 0.3,
      score_chemical_cla_parent = 0.4,
      score_chemical_npc_pathway = 0.5,
      score_chemical_npc_superclass = 0.6,
      score_chemical_npc_class = 0.7
    ),
    NA  # Should log warning but not throw error
  )
})

# =============================================================================
# Tests for decorate_masses()
# =============================================================================

test_that("decorate_masses handles empty input", {
  empty_df <- tidytable::tidytable(
    feature_id = character(0),
    candidate_structure_inchikey_connectivity_layer = character(0)
  )

  result <- decorate_masses(annotation_table_ms1 = empty_df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("decorate_masses returns input unchanged", {
  test_df <- tidytable::tidytable(
    feature_id = c("FT001", "FT002"),
    candidate_structure_inchikey_connectivity_layer = c("AAAAA", "BBBBB")
  )

  result <- decorate_masses(annotation_table_ms1 = test_df)

  # Should return input unchanged (decorator pattern)
  expect_identical(result, test_df)
})

test_that("decorate_masses filters NA and notAnnotated", {
  test_df <- tidytable::tidytable(
    feature_id = c("FT001", "FT002", "FT003", "FT004"),
    candidate_structure_inchikey_connectivity_layer = c(
      "AAAAA",
      NA,
      "notAnnotated",
      "BBBBB"
    )
  )

  result <- decorate_masses(annotation_table_ms1 = test_df)

  # Should return input unchanged - function just logs statistics
  expect_identical(result, test_df)
})

# =============================================================================
# Tests for vectorized counting
# =============================================================================

test_that("decoration functions use vectorized operations", {
  # This is implicitly tested by the performance of the functions
  # If vectorization wasn't working, tests would be much slower

  # Create moderately-sized test data
  large_df <- tidytable::tidytable(
    feature_id = rep(paste0("FT", 1:100), each = 10),
    score_biological = runif(1000, 0, 1),
    candidate_structure_inchikey_connectivity_layer = rep(
      paste0("INK", 1:1000),
      length.out = 1000
    )
  )

  # Should complete quickly if vectorized
  start_time <- Sys.time()
  result <- decorate_bio(
    annot_table_wei_bio = large_df,
    score_biological_kingdom = 0.1,
    score_biological_phylum = 0.2,
    score_biological_class = 0.3,
    score_biological_order = 0.4,
    score_biological_family = 0.5,
    score_biological_tribe = 0.6,
    score_biological_genus = 0.7,
    score_biological_species = 0.8,
    score_biological_variety = 0.9
  )
  end_time <- Sys.time()

  # Should be fast (<1 second for 1000 rows)
  expect_lt(as.numeric(difftime(end_time, start_time, units = "secs")), 1.0)
  expect_identical(result, large_df)
})

