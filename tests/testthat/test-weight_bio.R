# ==============================================================================
# Test Suite: weight_bio
# ==============================================================================

library(testthat)
library(tima)

# ==============================================================================
# Test: Input Validation
# ==============================================================================

test_that("weight_bio validates data frame input", {
  expect_error(
    weight_bio(
      annot_table_wei_bio = "not_a_dataframe",
      org_tax_ott = tidytable::tidytable(),
      structure_organism_pairs_table = tidytable::tidytable()
    ),
    "must be a data frame"
  )
})

test_that("weight_bio handles empty data frame", {
  result <- weight_bio(
    annot_table_wei_bio = tidytable::tidytable(),
    org_tax_ott = tidytable::tidytable(),
    structure_organism_pairs_table = tidytable::tidytable()
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

# ==============================================================================
# Test: Basic Functionality
# ==============================================================================

test_that("weight_bio processes valid input", {
  skip_on_cran()

  annot_table <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("STRUCT1", "STRUCT2"),
    score_biological = c(NA_real_, NA_real_)
  )

  org_tax <- tidytable::tidytable(
    organism_name = c("Plant1", "Plant2"),
    organism_taxonomy_ottid = c("123", "456"),
    organism_taxonomy_01domain = c("Eukaryota", "Eukaryota"),
    organism_taxonomy_02kingdom = c("Plantae", "Plantae"),
    organism_taxonomy_03phylum = c("Phylum1", "Phylum2")
  )

  struct_org_pairs <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = c("STRUCT1", "STRUCT2"),
    organism_name = c("Plant1", "Plant2"),
    organism_taxonomy_ottid = c("123", "456")
  )

  result <- weight_bio(
    annot_table_wei_bio = annot_table,
    org_tax_ott = org_tax,
    structure_organism_pairs_table = struct_org_pairs,
    weight_bio = 0.5,
    minimal_ms1_bio = 0.1
  )

  expect_s3_class(result, "data.frame")
  expect_true("score_biological" %in% names(result))
  expect_equal(nrow(result), 2L)
})

# ==============================================================================
# Test: Parameter Validation
# ==============================================================================

test_that("weight_bio validates weight_bio parameter", {
  skip_on_cran()

  annot_table <- tidytable::tidytable(
    feature_id = "F1",
    candidate_structure_inchikey_connectivity_layer = "STRUCT1"
  )

  expect_error(
    weight_bio(
      annot_table_wei_bio = annot_table,
      org_tax_ott = tidytable::tidytable(),
      structure_organism_pairs_table = tidytable::tidytable(),
      weight_bio = -0.5
    ),
    "between 0 and 1|positive"
  )

  expect_error(
    weight_bio(
      annot_table_wei_bio = annot_table,
      org_tax_ott = tidytable::tidytable(),
      structure_organism_pairs_table = tidytable::tidytable(),
      weight_bio = 1.5
    ),
    "between 0 and 1"
  )
})

test_that("weight_bio validates minimal_ms1_bio parameter", {
  skip_on_cran()

  annot_table <- tidytable::tidytable(
    feature_id = "F1",
    candidate_structure_inchikey_connectivity_layer = "STRUCT1"
  )

  expect_error(
    weight_bio(
      annot_table_wei_bio = annot_table,
      org_tax_ott = tidytable::tidytable(),
      structure_organism_pairs_table = tidytable::tidytable(),
      minimal_ms1_bio = -0.1
    ),
    "between 0 and 1|non-negative"
  )
})

# ==============================================================================
# Test: Score Calculation
# ==============================================================================

test_that("weight_bio calculates biological scores correctly", {
  skip_on_cran()
  skip("Requires full implementation details")

  # This test would verify that biological scores are calculated
  # correctly based on taxonomy distance
})

# ==============================================================================
# Test: Edge Cases
# ==============================================================================

test_that("weight_bio handles missing organism taxonomy", {
  skip_on_cran()

  annot_table <- tidytable::tidytable(
    feature_id = "F1",
    candidate_structure_inchikey_connectivity_layer = "STRUCT1",
    score_biological = NA_real_
  )

  result <- weight_bio(
    annot_table_wei_bio = annot_table,
    org_tax_ott = tidytable::tidytable(),
    structure_organism_pairs_table = tidytable::tidytable()
  )

  expect_s3_class(result, "data.frame")
})

test_that("weight_bio handles NA values in scores", {
  skip_on_cran()

  annot_table <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("STRUCT1", "STRUCT2"),
    score_biological = c(NA_real_, 0.5)
  )

  result <- weight_bio(
    annot_table_wei_bio = annot_table,
    org_tax_ott = tidytable::tidytable(),
    structure_organism_pairs_table = tidytable::tidytable()
  )

  expect_s3_class(result, "data.frame")
  expect_true("score_biological" %in% names(result))
})
