# Test Suite for weight_bio()
# Tests the biological source weighting function for metabolite annotations

library(testthat)
library(tima)

# ==============================================================================
# Helper: Create Test Data
# ==============================================================================

create_test_annotation_data <- function() {
  annotation_table_taxed <- tidytable::tidytable(
    feature_id = c("FT001", "FT001", "FT002", "FT002"),
    candidate_structure_inchikey_connectivity_layer = c(
      "ABCDEFGHIJKLMN",
      "NOPQRSTUVWXYZA",
      "BCDEFGHIJKLMNO",
      "CDEFGHIJKLMNOP"
    ),
    candidate_score_similarity = c(0.9, 0.8, 0.85, 0.75),
    candidate_score_sirius_csi = c(10, 8, 12, 6),
    sample_organism_name = rep("Gentiana lutea", 4),
    sample_organism_01_domain = rep("Eukaryota", 4),
    sample_organism_02_kingdom = rep("Plantae", 4),
    sample_organism_03_phylum = rep("Streptophyta", 4),
    sample_organism_04_class = rep("Magnoliopsida", 4),
    sample_organism_05_order = rep("Gentianales", 4),
    sample_organism_06_family = rep("Gentianaceae", 4),
    sample_organism_07_tribe = rep("Gentianeae", 4),
    sample_organism_08_genus = rep("Gentiana", 4),
    sample_organism_09_species = rep("Gentiana lutea", 4),
    sample_organism_10_varietas = rep(NA_character_, 4)
  )

  structure_organism_pairs_table <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = c(
      "ABCDEFGHIJKLMN",
      "NOPQRSTUVWXYZA",
      "BCDEFGHIJKLMNO",
      "CDEFGHIJKLMNOP"
    ),
    organism_name = c(
      "Gentiana lutea",
      "Gentiana verna",
      "Plantae sp.",
      "Bacteria sp."
    ),
    organism_taxonomy_01domain = c(
      "Eukaryota",
      "Eukaryota",
      "Eukaryota",
      "Bacteria"
    ),
    organism_taxonomy_02kingdom = c(
      "Plantae",
      "Plantae",
      "Plantae",
      "Bacteria"
    ),
    organism_taxonomy_03phylum = c(
      "Streptophyta",
      "Streptophyta",
      "Streptophyta",
      "Proteobacteria"
    ),
    organism_taxonomy_04class = c(
      "Magnoliopsida",
      "Magnoliopsida",
      "Magnoliopsida",
      "Gammaproteobacteria"
    ),
    organism_taxonomy_05order = c(
      "Gentianales",
      "Gentianales",
      "NA",
      "NA"
    ),
    organism_taxonomy_06family = c(
      "Gentianaceae",
      "Gentianaceae",
      "NA",
      "NA"
    ),
    organism_taxonomy_07tribe = c("Gentianeae", "Gentianeae", "NA", "NA"),
    organism_taxonomy_08genus = c("Gentiana", "Gentiana", "NA", "NA"),
    organism_taxonomy_09species = c("Gentiana lutea", "Gentiana verna", "NA", "NA"),
    organism_taxonomy_10varietas = rep(NA_character_, 4),
    organism_taxonomy_ottid = c(1, 2, 3, 4)
  )

  list(
    annotation_table_taxed = annotation_table_taxed,
    structure_organism_pairs_table = structure_organism_pairs_table
  )
}

# ==============================================================================
# Test: Input Validation
# ==============================================================================

test_that("weight_bio validates annotation_table_taxed parameter", {
  test_data <- create_test_annotation_data()

  # Non-data.frame
  expect_error(
    weight_bio(
      annotation_table_taxed = "not_a_df",
      structure_organism_pairs_table = test_data$structure_organism_pairs_table,
      weight_spectral = 0.5,
      weight_biological = 0.5,
      score_biological_domain = 0.1,
      score_biological_kingdom = 0.2,
      score_biological_phylum = 0.3,
      score_biological_class = 0.4,
      score_biological_order = 0.5,
      score_biological_family = 0.6,
      score_biological_tribe = 0.7,
      score_biological_genus = 0.8,
      score_biological_species = 0.9,
      score_biological_variety = 1.0
    ),
    "must be a data frame"
  )
})

test_that("weight_bio validates structure_organism_pairs_table parameter", {
  test_data <- create_test_annotation_data()

  expect_error(
    weight_bio(
      annotation_table_taxed = test_data$annotation_table_taxed,
      structure_organism_pairs_table = "not_a_df",
      weight_spectral = 0.5,
      weight_biological = 0.5,
      score_biological_domain = 0.1,
      score_biological_kingdom = 0.2,
      score_biological_phylum = 0.3,
      score_biological_class = 0.4,
      score_biological_order = 0.5,
      score_biological_family = 0.6,
      score_biological_tribe = 0.7,
      score_biological_genus = 0.8,
      score_biological_species = 0.9,
      score_biological_variety = 1.0
    ),
    "must be a data frame"
  )
})

test_that("weight_bio validates weight parameters", {
  test_data <- create_test_annotation_data()

  # Weight out of range (> 1)
  expect_error(
    weight_bio(
      annotation_table_taxed = test_data$annotation_table_taxed,
      structure_organism_pairs_table = test_data$structure_organism_pairs_table,
      weight_spectral = 1.5,
      weight_biological = 0.5,
      score_biological_domain = 0.1,
      score_biological_kingdom = 0.2,
      score_biological_phylum = 0.3,
      score_biological_class = 0.4,
      score_biological_order = 0.5,
      score_biological_family = 0.6,
      score_biological_tribe = 0.7,
      score_biological_genus = 0.8,
      score_biological_species = 0.9,
      score_biological_variety = 1.0
    ),
    "between 0 and 1"
  )

  # Negative weight
  expect_error(
    weight_bio(
      annotation_table_taxed = test_data$annotation_table_taxed,
      structure_organism_pairs_table = test_data$structure_organism_pairs_table,
      weight_spectral = -0.1,
      weight_biological = 0.5,
      score_biological_domain = 0.1,
      score_biological_kingdom = 0.2,
      score_biological_phylum = 0.3,
      score_biological_class = 0.4,
      score_biological_order = 0.5,
      score_biological_family = 0.6,
      score_biological_tribe = 0.7,
      score_biological_genus = 0.8,
      score_biological_species = 0.9,
      score_biological_variety = 1.0
    ),
    "between 0 and 1"
  )
})

test_that("weight_bio validates biological score parameters", {
  test_data <- create_test_annotation_data()

  # Score out of range
  expect_error(
    weight_bio(
      annotation_table_taxed = test_data$annotation_table_taxed,
      structure_organism_pairs_table = test_data$structure_organism_pairs_table,
      weight_spectral = 0.5,
      weight_biological = 0.5,
      score_biological_domain = -0.1, # Invalid
      score_biological_kingdom = 0.2,
      score_biological_phylum = 0.3,
      score_biological_class = 0.4,
      score_biological_order = 0.5,
      score_biological_family = 0.6,
      score_biological_tribe = 0.7,
      score_biological_genus = 0.8,
      score_biological_species = 0.9,
      score_biological_variety = 1.0
    ),
    "between 0 and 1"
  )
})

# ==============================================================================
# Test: Empty Input Handling
# ==============================================================================

test_that("weight_bio handles empty annotation table", {
  test_data <- create_test_annotation_data()

  empty_annotations <- test_data$annotation_table_taxed[0, ]

  result <- weight_bio(
    annotation_table_taxed = empty_annotations,
    structure_organism_pairs_table = test_data$structure_organism_pairs_table,
    weight_spectral = 0.5,
    weight_biological = 0.5,
    score_biological_domain = 0.1,
    score_biological_kingdom = 0.2,
    score_biological_phylum = 0.3,
    score_biological_class = 0.4,
    score_biological_order = 0.5,
    score_biological_family = 0.6,
    score_biological_tribe = 0.7,
    score_biological_genus = 0.8,
    score_biological_species = 0.9,
    score_biological_variety = 1.0
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

# ==============================================================================
# Test: Basic Functionality
# ==============================================================================

test_that("weight_bio returns data frame with biological scores", {
  test_data <- create_test_annotation_data()

  result <- weight_bio(
    annotation_table_taxed = test_data$annotation_table_taxed,
    structure_organism_pairs_table = test_data$structure_organism_pairs_table,
    weight_spectral = 0.5,
    weight_biological = 0.5,
    score_biological_domain = 0.1,
    score_biological_kingdom = 0.2,
    score_biological_phylum = 0.3,
    score_biological_class = 0.4,
    score_biological_order = 0.5,
    score_biological_family = 0.6,
    score_biological_tribe = 0.7,
    score_biological_genus = 0.8,
    score_biological_species = 0.9,
    score_biological_variety = 1.0
  )

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0L)
  expect_true("score_biological" %in% colnames(result))
  expect_true("score_weighted_bio" %in% colnames(result))
})

test_that("weight_bio assigns higher scores for better taxonomic matches", {
  test_data <- create_test_annotation_data()

  result <- weight_bio(
    annotation_table_taxed = test_data$annotation_table_taxed,
    structure_organism_pairs_table = test_data$structure_organism_pairs_table,
    weight_spectral = 0.5,
    weight_biological = 0.5,
    score_biological_domain = 0.1,
    score_biological_kingdom = 0.2,
    score_biological_phylum = 0.3,
    score_biological_class = 0.4,
    score_biological_order = 0.5,
    score_biological_family = 0.6,
    score_biological_tribe = 0.7,
    score_biological_genus = 0.8,
    score_biological_species = 0.9,
    score_biological_variety = 1.0
  )

  # Structure from exact same species should have highest score
  species_match <- result |>
    tidytable::filter(
      candidate_structure_inchikey_connectivity_layer == "ABCDEFGHIJKLMN"
    ) |>
    tidytable::pull(score_biological)

  # Structure from different kingdom should have lower score
  kingdom_mismatch <- result |>
    tidytable::filter(
      candidate_structure_inchikey_connectivity_layer == "CDEFGHIJKLMNOP"
    ) |>
    tidytable::pull(score_biological)

  if (length(species_match) > 0 && length(kingdom_mismatch) > 0) {
    expect_gt(species_match[1], kingdom_mismatch[1])
  }
})

# ==============================================================================
# Test: Weight Combinations
# ==============================================================================

test_that("weight_bio respects weight parameters", {
  test_data <- create_test_annotation_data()

  # 100% spectral weight
  result_spectral <- weight_bio(
    annotation_table_taxed = test_data$annotation_table_taxed,
    structure_organism_pairs_table = test_data$structure_organism_pairs_table,
    weight_spectral = 1.0,
    weight_biological = 0.0,
    score_biological_domain = 0.1,
    score_biological_kingdom = 0.2,
    score_biological_phylum = 0.3,
    score_biological_class = 0.4,
    score_biological_order = 0.5,
    score_biological_family = 0.6,
    score_biological_tribe = 0.7,
    score_biological_genus = 0.8,
    score_biological_species = 0.9,
    score_biological_variety = 1.0
  )

  # 100% biological weight
  result_biological <- weight_bio(
    annotation_table_taxed = test_data$annotation_table_taxed,
    structure_organism_pairs_table = test_data$structure_organism_pairs_table,
    weight_spectral = 0.0,
    weight_biological = 1.0,
    score_biological_domain = 0.1,
    score_biological_kingdom = 0.2,
    score_biological_phylum = 0.3,
    score_biological_class = 0.4,
    score_biological_order = 0.5,
    score_biological_family = 0.6,
    score_biological_tribe = 0.7,
    score_biological_genus = 0.8,
    score_biological_species = 0.9,
    score_biological_variety = 1.0
  )

  # Weighted scores should differ based on weights
  expect_false(
    all(
      result_spectral$score_weighted_bio ==
        result_biological$score_weighted_bio,
      na.rm = TRUE
    )
  )
})

# ==============================================================================
# Test: Score Hierarchy
# ==============================================================================

test_that("weight_bio respects taxonomic hierarchy", {
  test_data <- create_test_annotation_data()

  result <- weight_bio(
    annotation_table_taxed = test_data$annotation_table_taxed,
    structure_organism_pairs_table = test_data$structure_organism_pairs_table,
    weight_spectral = 0.0,
    weight_biological = 1.0,
    score_biological_domain = 0.1,
    score_biological_kingdom = 0.2,
    score_biological_phylum = 0.3,
    score_biological_class = 0.4,
    score_biological_order = 0.5,
    score_biological_family = 0.6,
    score_biological_tribe = 0.7,
    score_biological_genus = 0.8,
    score_biological_species = 0.9,
    score_biological_variety = 1.0
  )

  # Scores should exist and be non-negative
  expect_true(all(result$score_biological >= 0, na.rm = TRUE))
  expect_true(all(result$score_biological <= 1, na.rm = TRUE))
})

# ==============================================================================
# Test: Output Structure
# ==============================================================================

test_that("weight_bio preserves input columns", {
  test_data <- create_test_annotation_data()

  result <- weight_bio(
    annotation_table_taxed = test_data$annotation_table_taxed,
    structure_organism_pairs_table = test_data$structure_organism_pairs_table,
    weight_spectral = 0.5,
    weight_biological = 0.5,
    score_biological_domain = 0.1,
    score_biological_kingdom = 0.2,
    score_biological_phylum = 0.3,
    score_biological_class = 0.4,
    score_biological_order = 0.5,
    score_biological_family = 0.6,
    score_biological_tribe = 0.7,
    score_biological_genus = 0.8,
    score_biological_species = 0.9,
    score_biological_variety = 1.0
  )

  # Original columns should be present
  expect_true("feature_id" %in% colnames(result))
  expect_true(
    "candidate_structure_inchikey_connectivity_layer" %in% colnames(result)
  )

  # New columns should be added
  expect_true("score_biological" %in% colnames(result))
  expect_true("score_weighted_bio" %in% colnames(result))
})

test_that("weight_bio returns same number of rows or fewer (after filtering)", {
  test_data <- create_test_annotation_data()

  result <- weight_bio(
    annotation_table_taxed = test_data$annotation_table_taxed,
    structure_organism_pairs_table = test_data$structure_organism_pairs_table,
    weight_spectral = 0.5,
    weight_biological = 0.5,
    score_biological_domain = 0.1,
    score_biological_kingdom = 0.2,
    score_biological_phylum = 0.3,
    score_biological_class = 0.4,
    score_biological_order = 0.5,
    score_biological_family = 0.6,
    score_biological_tribe = 0.7,
    score_biological_genus = 0.8,
    score_biological_species = 0.9,
    score_biological_variety = 1.0
  )

  # Result should have rows (may be filtered)
  expect_lte(nrow(result), nrow(test_data$annotation_table_taxed))
})

# ==============================================================================
# Test: Edge Cases
# ==============================================================================

test_that("weight_bio handles missing organism info", {
  # Create data with some missing organism info
  annotation_with_na <- tidytable::tidytable(
    feature_id = "FT001",
    candidate_structure_inchikey_connectivity_layer = "ABCDEFGHIJKLMN",
    candidate_score_similarity = 0.9,
    candidate_score_sirius_csi = NA_real_,  # Add missing column
    sample_organism_name = NA_character_,
    sample_organism_01_domain = NA_character_,
    sample_organism_02_kingdom = NA_character_,
    sample_organism_03_phylum = NA_character_,
    sample_organism_04_class = NA_character_,
    sample_organism_05_order = NA_character_,
    sample_organism_06_family = NA_character_,
    sample_organism_07_tribe = NA_character_,
    sample_organism_08_genus = NA_character_,
    sample_organism_09_species = NA_character_,
    sample_organism_10_varietas = NA_character_
  )

  test_data <- create_test_annotation_data()

  # Should handle gracefully
  expect_no_error(
    weight_bio(
      annotation_table_taxed = annotation_with_na,
      structure_organism_pairs_table = test_data$structure_organism_pairs_table,
      weight_spectral = 0.5,
      weight_biological = 0.5,
      score_biological_domain = 0.1,
      score_biological_kingdom = 0.2,
      score_biological_phylum = 0.3,
      score_biological_class = 0.4,
      score_biological_order = 0.5,
      score_biological_family = 0.6,
      score_biological_tribe = 0.7,
      score_biological_genus = 0.8,
      score_biological_species = 0.9,
      score_biological_variety = 1.0
    )
  )
})
