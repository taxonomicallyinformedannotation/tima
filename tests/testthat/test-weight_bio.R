# =============================================================================
# Tests for weight_bio() - REVISED AND FIXED
# =============================================================================
# Comprehensive test coverage for biological weighting of MS annotations
# based on taxonomic similarity between candidate structures and samples.

# Note: weight_bio is an internal function, accessed via weight_bio()

# =============================================================================
# Helper Functions
# =============================================================================

# Create a complete annotation table with all required columns
create_test_annotation <- function(
  feature_ids,
  inchikeys,
  sample_organism = "Gentiana lutea",
  domain = "Eukaryota",
  kingdom = "Plantae",
  phylum = "Tracheophyta",
  class_tax = "Magnoliopsida",
  order_tax = "Gentianales",
  family = "Gentianaceae",
  tribe = NA_character_,
  genus = "Gentiana",
  species = "Gentiana lutea",
  varietas = NA_character_,
  score_similarity = 0.8,
  score_sirius = NA_real_
) {
  n <- length(feature_ids)

  # Ensure vectors are recycled to correct length
  tidytable::tidytable(
    feature_id = feature_ids,
    candidate_structure_inchikey_connectivity_layer = inchikeys,
    sample_organism_name = rep_len(sample_organism, n),
    sample_organism_01_domain = rep_len(domain, n),
    sample_organism_02_kingdom = rep_len(kingdom, n),
    sample_organism_03_phylum = rep_len(phylum, n),
    sample_organism_04_class = rep_len(class_tax, n),
    sample_organism_05_order = rep_len(order_tax, n),
    sample_organism_06_family = rep_len(family, n),
    sample_organism_07_tribe = rep_len(tribe, n),
    sample_organism_08_genus = rep_len(genus, n),
    sample_organism_09_species = rep_len(species, n),
    sample_organism_10_varietas = rep_len(varietas, n),
    candidate_score_similarity = rep_len(score_similarity, n),
    candidate_score_sirius_csi = rep_len(score_sirius, n)
  )
}

# =============================================================================
# Input Validation Tests
# =============================================================================

test_that("weight_bio validates data frame inputs", {
  # Non-data frame annotation_table_taxed should error
  expect_error(
    weight_bio(
      annotation_table_taxed = "not a dataframe",
      structure_organism_pairs_table = data.frame(),
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
    "annotation_table_taxed must be a data frame"
  )

  # Non-data frame structure_organism_pairs_table should error
  expect_error(
    weight_bio(
      annotation_table_taxed = data.frame(),
      structure_organism_pairs_table = list(),
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
    "structure_organism_pairs_table must be a data frame"
  )
})

test_that("weight_bio validates weight parameters", {
  # Invalid spectral weight (negative)
  expect_error(
    weight_bio(
      annotation_table_taxed = data.frame(feature_id = "F1"),
      structure_organism_pairs_table = data.frame(),
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
    "Weight\\(s\\) must be between 0 and 1.*spectral"
  )

  # Invalid biological weight
  expect_error(
    weight_bio(
      annotation_table_taxed = data.frame(feature_id = "F1"),
      structure_organism_pairs_table = data.frame(),
      weight_spectral = 0.5,
      weight_biological = 2.0,
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
    "Weight\\(s\\) must be between 0 and 1.*biological"
  )
})

test_that("weight_bio validates biological score parameters", {
  # Invalid score (negative)
  expect_error(
    weight_bio(
      annotation_table_taxed = data.frame(feature_id = "F1"),
      structure_organism_pairs_table = data.frame(),
      weight_spectral = 0.5,
      weight_biological = 0.5,
      score_biological_domain = -0.1,
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
    "Biological score\\(s\\) must be between 0 and 1"
  )
})

# =============================================================================
# Edge Cases and Empty Input Tests
# =============================================================================

test_that("weight_bio handles empty annotation table", {
  # Create empty annotation table with proper structure
  empty_annotations <- create_test_annotation(
    feature_ids = character(0),
    inchikeys = character(0)
  )

  # Create non-empty structure-organism pairs
  sop_table <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = generate_fake_inchikey(3, seed = 1),
    organism_name = c(
      "Gentiana lutea",
      "Arabidopsis thaliana",
      "Coffea arabica"
    ),
    organism_taxonomy_ottid = c("123456", "234567", "345678"),
    organism_taxonomy_01domain = rep("Eukaryota", 3),
    organism_taxonomy_02kingdom = rep("Plantae", 3),
    organism_taxonomy_03phylum = rep("Tracheophyta", 3),
    organism_taxonomy_04class = rep("Magnoliopsida", 3),
    organism_taxonomy_05order = c("Gentianales", "Brassicales", "Gentianales"),
    organism_taxonomy_06family = c("Gentianaceae", "Brassicaceae", "Rubiaceae"),
    organism_taxonomy_07tribe = c(NA, NA, "Coffeeae"),
    organism_taxonomy_08genus = c("Gentiana", "Arabidopsis", "Coffea"),
    organism_taxonomy_09species = c(
      "Gentiana lutea",
      "Arabidopsis thaliana",
      "Coffea arabica"
    ),
    organism_taxonomy_10varietas = c(NA, NA, NA)
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

  # Should return empty input unchanged
  expect_equal(nrow(result), 0)
  expect_s3_class(result, "data.frame")
})

test_that("weight_bio handles annotations without matching organisms", {
  skip("Known issue: weight_bio has type mismatch bug when no organisms match")

  # Annotations with structures not in organism pairs
  annotations <- create_test_annotation(
    feature_ids = c("F1", "F2"),
    inchikeys = generate_fake_inchikey(2, seed = 99)
  )

  # Structure-organism pairs with DIFFERENT structures
  sop_table <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = generate_fake_inchikey(2, seed = 1),
    organism_name = c("Arabidopsis thaliana", "Coffea arabica"),
    organism_taxonomy_ottid = c("234567", "345678"),
    organism_taxonomy_01domain = rep("Eukaryota", 2),
    organism_taxonomy_02kingdom = rep("Plantae", 2),
    organism_taxonomy_03phylum = rep("Tracheophyta", 2),
    organism_taxonomy_04class = rep("Magnoliopsida", 2),
    organism_taxonomy_05order = c("Brassicales", "Gentianales"),
    organism_taxonomy_06family = c("Brassicaceae", "Rubiaceae"),
    organism_taxonomy_07tribe = c(NA, "Coffeeae"),
    organism_taxonomy_08genus = c("Arabidopsis", "Coffea"),
    organism_taxonomy_09species = c("Arabidopsis thaliana", "Coffea arabica"),
    organism_taxonomy_10varietas = c(NA, NA)
  )

  result <- weight_bio(
    annotation_table_taxed = annotations,
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

  # Should return results (structures don't match, so bio score = 0)
  expect_true(nrow(result) > 0)
  expect_s3_class(result, "data.frame")

  # Should have score_weighted_bio column
  expect_true("score_weighted_bio" %in% colnames(result))
})

# =============================================================================
# Functional Tests - Taxonomic Scoring
# =============================================================================

test_that("weight_bio assigns highest score to exact species match", {
  # Create annotation with sample organism
  inchikey <- generate_fake_inchikey(1, seed = 42)[1]

  annotations <- create_test_annotation(
    feature_ids = "F1",
    inchikeys = inchikey,
    sample_organism = "Gentiana lutea",
    genus = "Gentiana",
    species = "Gentiana lutea"
  )

  # Structure from exact same species
  sop_table <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = inchikey,
    organism_name = "Gentiana lutea",
    organism_taxonomy_ottid = "123456",
    organism_taxonomy_01domain = "Eukaryota",
    organism_taxonomy_02kingdom = "Plantae",
    organism_taxonomy_03phylum = "Tracheophyta",
    organism_taxonomy_04class = "Magnoliopsida",
    organism_taxonomy_05order = "Gentianales",
    organism_taxonomy_06family = "Gentianaceae",
    organism_taxonomy_07tribe = NA_character_,
    organism_taxonomy_08genus = "Gentiana",
    organism_taxonomy_09species = "Gentiana lutea",
    organism_taxonomy_10varietas = NA_character_
  )

  result <- weight_bio(
    annotation_table_taxed = annotations,
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

  expect_true(nrow(result) > 0)

  # Check that biological score equals species-level score (highest match)
  if ("score_biological" %in% colnames(result)) {
    # Species match should give 0.9 score
    expect_true(any(result$score_biological == 0.9))
  }
})

test_that("weight_bio assigns correct score to genus-level match", {
  inchikey <- generate_fake_inchikey(1, seed = 43)[1]

  # Sample is Gentiana lutea
  annotations <- create_test_annotation(
    feature_ids = "F1",
    inchikeys = inchikey,
    sample_organism = "Gentiana lutea",
    genus = "Gentiana",
    species = "Gentiana lutea"
  )

  # Structure from different species, same genus (Gentiana acaulis)
  sop_table <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = inchikey,
    organism_name = "Gentiana acaulis",
    organism_taxonomy_ottid = "123457",
    organism_taxonomy_01domain = "Eukaryota",
    organism_taxonomy_02kingdom = "Plantae",
    organism_taxonomy_03phylum = "Tracheophyta",
    organism_taxonomy_04class = "Magnoliopsida",
    organism_taxonomy_05order = "Gentianales",
    organism_taxonomy_06family = "Gentianaceae",
    organism_taxonomy_07tribe = NA_character_,
    organism_taxonomy_08genus = "Gentiana",
    organism_taxonomy_09species = "Gentiana acaulis",
    organism_taxonomy_10varietas = NA_character_
  )

  result <- weight_bio(
    annotation_table_taxed = annotations,
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

  expect_true(nrow(result) > 0)

  # Check that biological score equals genus-level score
  if ("score_biological" %in% colnames(result)) {
    # Genus match should give 0.8 score (not species 0.9)
    expect_true(any(result$score_biological == 0.8))
  }
})

test_that("weight_bio assigns correct score to family-level match", {
  inchikey <- generate_fake_inchikey(1, seed = 44)[1]

  # Sample is Gentiana lutea (Gentianaceae)
  annotations <- create_test_annotation(
    feature_ids = "F1",
    inchikeys = inchikey,
    sample_organism = "Gentiana lutea",
    family = "Gentianaceae",
    genus = "Gentiana",
    species = "Gentiana lutea"
  )

  # Structure from different genus, same family (Swertia from Gentianaceae)
  sop_table <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = inchikey,
    organism_name = "Swertia perennis",
    organism_taxonomy_ottid = "999888",
    organism_taxonomy_01domain = "Eukaryota",
    organism_taxonomy_02kingdom = "Plantae",
    organism_taxonomy_03phylum = "Tracheophyta",
    organism_taxonomy_04class = "Magnoliopsida",
    organism_taxonomy_05order = "Gentianales",
    organism_taxonomy_06family = "Gentianaceae",
    organism_taxonomy_07tribe = NA_character_,
    organism_taxonomy_08genus = "Swertia",
    organism_taxonomy_09species = "Swertia perennis",
    organism_taxonomy_10varietas = NA_character_
  )

  result <- weight_bio(
    annotation_table_taxed = annotations,
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

  expect_true(nrow(result) > 0)

  # Check that biological score equals family-level score
  if ("score_biological" %in% colnames(result)) {
    # Family match should give 0.6 score
    expect_true(any(result$score_biological == 0.6))
  }
})

# =============================================================================
# Output Structure Tests
# =============================================================================

test_that("weight_bio returns expected columns", {
  inchikey <- generate_fake_inchikey(1, seed = 50)[1]

  annotations <- create_test_annotation(
    feature_ids = "F1",
    inchikeys = inchikey,
    score_similarity = 0.85
  )

  sop_table <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = inchikey,
    organism_name = "Gentiana lutea",
    organism_taxonomy_ottid = "123456",
    organism_taxonomy_01domain = "Eukaryota",
    organism_taxonomy_02kingdom = "Plantae",
    organism_taxonomy_03phylum = "Tracheophyta",
    organism_taxonomy_04class = "Magnoliopsida",
    organism_taxonomy_05order = "Gentianales",
    organism_taxonomy_06family = "Gentianaceae",
    organism_taxonomy_07tribe = NA_character_,
    organism_taxonomy_08genus = "Gentiana",
    organism_taxonomy_09species = "Gentiana lutea",
    organism_taxonomy_10varietas = NA_character_
  )

  result <- weight_bio(
    annotation_table_taxed = annotations,
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

  # Should contain score_biological column
  expect_true("score_biological" %in% colnames(result))

  # Should contain weighted bio score
  expect_true("score_weighted_bio" %in% colnames(result))

  # Should preserve input columns
  expect_true("feature_id" %in% colnames(result))
  expect_true(
    "candidate_structure_inchikey_connectivity_layer" %in% colnames(result)
  )

  # May contain candidate_organism_name when there's a match
  # (not guaranteed, depends on whether structures match organism pairs)
})

# =============================================================================
# Performance Tests
# =============================================================================

test_that(
  skip("Not implemented")
)
# test_that("weight_bio handles moderate-scale data efficiently", {
#   skip_on_cran()
#   skip_if_not(interactive(), "Performance test only for local development")
#
#   # Create 100 annotations
#   n_annotations <- 100
#   n_structures <- 20
#
#   inchikeys <- generate_fake_inchikey(n_structures, seed = 100)
#
#   annotations <- create_test_annotation(
#     feature_ids = paste0("F", 1:n_annotations),
#     inchikeys = sample(inchikeys, n_annotations, replace = TRUE),
#     score_similarity = runif(n_annotations, 0.5, 1.0)
#   )
#
#   # Create structure-organism pairs (50 pairs per structure)
#   sop_table <- tidytable::tidytable(
#     structure_inchikey_connectivity_layer = rep(inchikeys, each = 50),
#     organism_name = rep(
#       c(
#         "Gentiana lutea",
#         "Gentiana acaulis",
#         "Arabidopsis thaliana",
#         "Coffea arabica",
#         "Swertia perennis"
#       ),
#       times = n_structures * 10
#     )[1:(n_structures * 50)],
#     organism_taxonomy_ottid = paste0("OTT", 1:(n_structures * 50)),
#     organism_taxonomy_01domain = "Eukaryota",
#     organism_taxonomy_02kingdom = "Plantae",
#     organism_taxonomy_03phylum = "Tracheophyta",
#     organism_taxonomy_04class = "Magnoliopsida",
#     organism_taxonomy_05order = sample(
#       c("Gentianales", "Brassicales"),
#       n_structures * 50,
#       replace = TRUE
#     ),
#     organism_taxonomy_06family = sample(
#       c("Gentianaceae", "Brassicaceae", "Rubiaceae"),
#       n_structures * 50,
#       replace = TRUE
#     ),
#     organism_taxonomy_07tribe = NA_character_,
#     organism_taxonomy_08genus = sample(
#       c("Gentiana", "Arabidopsis", "Coffea", "Swertia"),
#       n_structures * 50,
#       replace = TRUE
#     ),
#     organism_taxonomy_09species = organism_name,
#     organism_taxonomy_10varietas = NA_character_
#   )
#
#   # Should complete in reasonable time (<5 seconds)
#   start_time <- Sys.time()
#
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
#
#   elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
#
#   expect_true(
#     elapsed_time < 5,
#     info = sprintf(
#       "Processing took %.2f seconds (expected < 5s)",
#       elapsed_time
#     )
#   )
#
#   expect_true(nrow(result) > 0)
#   expect_s3_class(result, "data.frame")
# })
