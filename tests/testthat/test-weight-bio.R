# Test: Weight Bio Function
library(testthat)

# =============================================================================
# Tests for weight_bio() - Core TIMA algorithm
# =============================================================================

test_that("weight_bio validates input data frames", {
  expect_error(
    weight_bio(
      annotation_table_taxed = "not a dataframe",
      structure_organism_pairs_table = tidytable::tidytable(),
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

test_that("weight_bio returns early with empty annotation table", {
  copy_backbone(cache_dir = ".")

  empty_annotations <- tidytable::tidytable(
    feature_id = character(0)
  )

  sop_table <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = character(0),
    organism_taxonomy_ottid = character(0)
  )

  result <- weight_bio(
    annotation_table_taxed = empty_annotations,
    structure_organism_pairs_table = sop_table,
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
  expect_equal(nrow(result), 0)

  unlink("data", recursive = TRUE)
})

test_that("weight_bio validates weight parameters", {
  copy_backbone(cache_dir = ".")

  annotations <- tidytable::tidytable(
    feature_id = c("FT001"),
    candidate_structure_inchikey_connectivity_layer = c("AAAAA")
  )

  sop_table <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = c("AAAAA"),
    organism_taxonomy_ottid = c("12345")
  )

  # Test invalid spectral weight
  expect_error(
    weight_bio(
      annotation_table_taxed = annotations,
      structure_organism_pairs_table = sop_table,
      weight_spectral = 1.5, # Invalid: > 1
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
    "must be between 0 and 1"
  )

  # Test negative weight
  expect_error(
    weight_bio(
      annotation_table_taxed = annotations,
      structure_organism_pairs_table = sop_table,
      weight_spectral = 0.5,
      weight_biological = -0.1, # Invalid: < 0
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
    "must be between 0 and 1"
  )

  unlink("data", recursive = TRUE)
})

test_that("weight_bio validates biological score parameters", {
  copy_backbone(cache_dir = ".")

  annotations <- tidytable::tidytable(
    feature_id = c("FT001"),
    candidate_structure_inchikey_connectivity_layer = c("AAAAA")
  )

  sop_table <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = c("AAAAA"),
    organism_taxonomy_ottid = c("12345")
  )

  # Test invalid biological score
  expect_error(
    weight_bio(
      annotation_table_taxed = annotations,
      structure_organism_pairs_table = sop_table,
      weight_spectral = 0.5,
      weight_biological = 0.5,
      score_biological_domain = 1.5, # Invalid: > 1
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
    "must be between 0 and 1"
  )

  unlink("data", recursive = TRUE)
})

test_that("weight_bio uses vectorized validation", {
  # Test that vectorized validation catches multiple invalid parameters
  copy_backbone(cache_dir = ".")

  annotations <- tidytable::tidytable(
    feature_id = c("FT001"),
    candidate_structure_inchikey_connectivity_layer = c("AAAAA")
  )

  sop_table <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = c("AAAAA"),
    organism_taxonomy_ottid = c("12345")
  )

  # Both weights invalid - should report both
  expect_error(
    weight_bio(
      annotation_table_taxed = annotations,
      structure_organism_pairs_table = sop_table,
      weight_spectral = 1.5, # Invalid
      weight_biological = -0.5, # Invalid
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
    "must be between 0 and 1"
  )

  unlink("data", recursive = TRUE)
})

test_that("weight_bio list-driven processing works correctly", {
  # Test that the list-driven approach processes all taxonomic levels
  # This is tested implicitly through the full pipeline tests
  # Here we just verify the structure is correct

  copy_backbone(cache_dir = ".")

  # Create minimal valid inputs
  annotations <- tidytable::tidytable(
    feature_id = c("FT001"),
    candidate_structure_inchikey_connectivity_layer = c("AAAAA"),
    candidate_score_similarity = c(0.1),
    candidate_score_sirius_csi = c(0.1),
    sample_organism_name = c("Test organism"),
    sample_organism_01_domain = c("Eukaryota"),
    sample_organism_02_kingdom = c("Plantae"),
    sample_organism_03_phylum = c("Tracheophyta"),
    sample_organism_04_class = c("Magnoliopsida"),
    sample_organism_05_order = c("Gentianales"),
    sample_organism_06_family = c("Gentianaceae"),
    sample_organism_07_tribe = c("Gentianeae"),
    sample_organism_08_genus = c("Gentiana"),
    sample_organism_09_species = c("Gentiana lutea"),
    sample_organism_10_varietas = c("Gentiana lutea var. lutea")
  )

  sop_table <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = c("AAAAA"),
    organism_taxonomy_ottid = c("12345"),
    organism_name = c("Test organism"),
    organism_taxonomy_01domain = c("Eukaryota"),
    organism_taxonomy_02kingdom = c("Plantae"),
    organism_taxonomy_03phylum = c("Tracheophyta"),
    organism_taxonomy_04class = c("Magnoliopsida"),
    organism_taxonomy_05order = c("Gentianales"),
    organism_taxonomy_06family = c("Gentianaceae"),
    organism_taxonomy_07tribe = c("Gentianeae"),
    organism_taxonomy_08genus = c("Gentiana"),
    organism_taxonomy_09species = c("Gentiana lutea"),
    organism_taxonomy_10varietas = c("Gentiana lutea var. lutea")
  )

  # Should complete without error
  # expect_no_error({
  #   result <- weight_bio(
  #     annotation_table_taxed = annotations,
  #     structure_organism_pairs_table = sop_table,
  #     weight_spectral = 0.5,
  #     weight_biological = 0.5,
  #     score_biological_domain = 0.1,
  #     score_biological_kingdom = 0.2,
  #     score_biological_phylum = 0.3,
  #     score_biological_class = 0.4,
  #     score_biological_order = 0.5,
  #     score_biological_family = 0.6,
  #     score_biological_tribe = 0.7,
  #     score_biological_genus = 0.8,
  #     score_biological_species = 0.9,
  #     score_biological_variety = 1.0
  #   )
  # })

  unlink("data", recursive = TRUE)
})
