# Test Suite: weight_bio ----

library(testthat)


## Internal Utility Helpers ----

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

## Input Validation ----

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
      score_biological_variety = 1.0,
      score_biological_biota = 1.007276
    ),
    "Fix: Ensure input is a valid data frame",
    fixed = TRUE
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
      score_biological_variety = 1.0,
      score_biological_biota = 1.007276
    ),
    "Fix: Ensure input is a valid data frame",
    fixed = TRUE
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
      score_biological_variety = 1.0,
      score_biological_biota = 1.007276
    ),
    "weights must be non-negative. Negative weight(s): position 1",
    fixed = TRUE
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
      score_biological_variety = 1.0,
      score_biological_biota = 1.007276
    ),
    "Weight(s) must be between 0 and 1: biological",
    fixed = TRUE
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
      score_biological_variety = 1.0,
      score_biological_biota = 1.007276
    ),
    "Fix: Use a value between 0 and 1",
    fixed = TRUE
  )
})

## Edge Cases and Empty Input ----

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
    score_biological_variety = 1.0,
    score_biological_biota = 1.007276
  )

  # Should return empty input unchanged
  expect_equal(nrow(result), 0)
  expect_s3_class(result, "data.frame")
})

## Functional Tests - Taxonomic Scoring ----

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
    score_biological_variety = 1.0,
    score_biological_biota = 1.007276
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
    score_biological_variety = 1.0,
    score_biological_biota = 1.007276
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
    score_biological_variety = 1.0,
    score_biological_biota = 1.007276
  )

  expect_true(nrow(result) > 0)

  # Check that biological score equals family-level score
  if ("score_biological" %in% colnames(result)) {
    # Family match should give 0.6 score
    expect_true(any(result$score_biological == 0.6))
  }
})

## Output Structure ----

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
    score_biological_variety = 1.0,
    score_biological_biota = 1.007276
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

## Performance ----

test_that("weight_bio handles annotations without matching organisms", {
  # Annotations with structures not in organism pairs
  annotations <- create_test_annotation(
    feature_ids = c("F1", "F2"),
    inchikeys = generate_fake_inchikey(2, seed = 99)
  )

  # Structure-organism pairs with DIFFERENT structures (no matches)
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
    score_biological_variety = 1.0,
    score_biological_biota = 1.007276
  )

  # Should return results
  expect_true(nrow(result) > 0)

  # When structures don't match organism pairs, biological score should be 0
  expect_true("score_biological" %in% names(result))
  expect_true(all(
    result$score_biological == 0 | is.na(result$score_biological)
  ))

  # Weighted score should exist and be based only on spectral component
  expect_true("score_weighted_bio" %in% names(result))
})

## Biota Domain Special Handling Tests ----

test_that("weight_bio gives maximum score (1.0) to Biota domain candidates", {
  # Create annotation for a random sample organism
  annotations <- create_test_annotation(
    feature_ids = "F1",
    inchikeys = "BDAGIHXWWSANSR", # Glucose-6-phosphate
    sample_organism = "Homo sapiens",
    domain = "Eukaryota",
    kingdom = "Metazoa"
  )

  # Create SOP table with Biota organism (shared core metabolism)
  sop_biota <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = "BDAGIHXWWSANSR",
    organism_name = "Biota",
    organism_taxonomy_ottid = 0L,
    organism_taxonomy_01domain = "Biota",
    organism_taxonomy_02kingdom = NA_character_,
    organism_taxonomy_03phylum = NA_character_,
    organism_taxonomy_04class = NA_character_,
    organism_taxonomy_05order = NA_character_,
    organism_taxonomy_06family = NA_character_,
    organism_taxonomy_07tribe = NA_character_,
    organism_taxonomy_08genus = NA_character_,
    organism_taxonomy_09species = NA_character_,
    organism_taxonomy_10varietas = NA_character_
  )

  result <- weight_bio(
    annotation_table_taxed = annotations,
    structure_organism_pairs_table = sop_biota,
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
    score_biological_variety = 1.0,
    score_biological_biota = 1.007276
  )

  # Biota candidates should get maximum biological score
  expect_equal(result$score_biological[[1]], 1.007276)

  # Closest occurrence should be "Biota"
  expect_equal(
    result$candidate_structure_organism_occurrence_closest[[1]],
    "Biota"
  )
})

test_that("Biota domain overrides taxonomic mismatch", {
  # Sample from bacteria
  annotations <- create_test_annotation(
    feature_ids = "F1",
    inchikeys = "BDAGIHXWWSANSR",
    sample_organism = "Escherichia coli",
    domain = "Bacteria",
    kingdom = NA_character_,
    phylum = "Proteobacteria"
  )

  # Candidate from Biota (should still get max score despite domain mismatch)
  sop_biota <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = "BDAGIHXWWSANSR",
    organism_name = "Biota",
    organism_taxonomy_ottid = 0L,
    organism_taxonomy_01domain = "Biota",
    organism_taxonomy_02kingdom = NA_character_,
    organism_taxonomy_03phylum = NA_character_,
    organism_taxonomy_04class = NA_character_,
    organism_taxonomy_05order = NA_character_,
    organism_taxonomy_06family = NA_character_,
    organism_taxonomy_07tribe = NA_character_,
    organism_taxonomy_08genus = NA_character_,
    organism_taxonomy_09species = NA_character_,
    organism_taxonomy_10varietas = NA_character_
  )

  result <- weight_bio(
    annotation_table_taxed = annotations,
    structure_organism_pairs_table = sop_biota,
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
    score_biological_variety = 1.0,
    score_biological_biota = 1.007276
  )

  # Should still get max score despite being from different domain
  expect_equal(result$score_biological[[1]], 1.007276)
})

test_that("Biota takes precedence over exact species match", {
  # Create annotation
  annotations <- create_test_annotation(
    feature_ids = c("F1", "F2"),
    inchikeys = rep("BDAGIHXWWSANSR", 2)
  )

  # Create two SOPs: one Biota, one exact species match
  sop_table <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = rep("BDAGIHXWWSANSR", 2),
    organism_name = c("Biota", "Gentiana lutea"),
    organism_taxonomy_ottid = c(0L, 123456L),
    organism_taxonomy_01domain = c("Biota", "Eukaryota"),
    organism_taxonomy_02kingdom = c(NA_character_, "Plantae"),
    organism_taxonomy_03phylum = c(NA_character_, "Tracheophyta"),
    organism_taxonomy_04class = c(NA_character_, "Magnoliopsida"),
    organism_taxonomy_05order = c(NA_character_, "Gentianales"),
    organism_taxonomy_06family = c(NA_character_, "Gentianaceae"),
    organism_taxonomy_07tribe = c(NA_character_, NA_character_),
    organism_taxonomy_08genus = c(NA_character_, "Gentiana"),
    organism_taxonomy_09species = c(NA_character_, "Gentiana lutea"),
    organism_taxonomy_10varietas = c(NA_character_, NA_character_)
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
    score_biological_variety = 1.0,
    score_biological_biota = 1.007276
  )

  # Both should get max score (Biota = 1.0, exact species = 0.9)
  expect_true(all(result$score_biological %in% c(0.9, 1.007276)))

  # At least one should be Biota
  expect_true(
    "Biota" %in% result$candidate_structure_organism_occurrence_closest
  )
})

test_that("Multiple Biota metabolites all get maximum score", {
  # Create annotations for multiple core metabolites
  core_metabolites <- c(
    "BDAGIHXWWSANSR", # Glucose-6-phosphate
    "LCTONWCANYUPML", # Pyruvate
    "KRKNYBCHXYNGOX" # Citrate
  )

  annotations <- create_test_annotation(
    feature_ids = paste0("F", 1:3),
    inchikeys = core_metabolites,
    sample_organism = "Arabidopsis thaliana",
    domain = "Eukaryota",
    kingdom = "Plantae"
  )

  # All from Biota
  sop_biota <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = core_metabolites,
    organism_name = rep("Biota", 3),
    organism_taxonomy_ottid = rep(0L, 3),
    organism_taxonomy_01domain = rep("Biota", 3),
    organism_taxonomy_02kingdom = rep(NA_character_, 3),
    organism_taxonomy_03phylum = rep(NA_character_, 3),
    organism_taxonomy_04class = rep(NA_character_, 3),
    organism_taxonomy_05order = rep(NA_character_, 3),
    organism_taxonomy_06family = rep(NA_character_, 3),
    organism_taxonomy_07tribe = rep(NA_character_, 3),
    organism_taxonomy_08genus = rep(NA_character_, 3),
    organism_taxonomy_09species = rep(NA_character_, 3),
    organism_taxonomy_10varietas = rep(NA_character_, 3)
  )

  result <- weight_bio(
    annotation_table_taxed = annotations,
    structure_organism_pairs_table = sop_biota,
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
    score_biological_variety = 1.0,
    score_biological_biota = 1.007276
  )

  # All should get maximum biological score
  expect_true(all(result$score_biological == 1.007276))

  # All should show Biota as closest occurrence
  expect_true(all(
    result$candidate_structure_organism_occurrence_closest == "Biota"
  ))
})

test_that("Biota works with empty taxonomic levels", {
  annotations <- create_test_annotation(
    feature_ids = "F1",
    inchikeys = "BDAGIHXWWSANSR"
  )

  # Biota with explicitly NA taxonomy (as it should be)
  sop_biota <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = "BDAGIHXWWSANSR",
    organism_name = "Biota",
    organism_taxonomy_ottid = 0L,
    organism_taxonomy_01domain = "Biota",
    organism_taxonomy_02kingdom = NA_character_,
    organism_taxonomy_03phylum = NA_character_,
    organism_taxonomy_04class = NA_character_,
    organism_taxonomy_05order = NA_character_,
    organism_taxonomy_06family = NA_character_,
    organism_taxonomy_07tribe = NA_character_,
    organism_taxonomy_08genus = NA_character_,
    organism_taxonomy_09species = NA_character_,
    organism_taxonomy_10varietas = NA_character_
  )

  result <- weight_bio(
    annotation_table_taxed = annotations,
    structure_organism_pairs_table = sop_biota,
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
    score_biological_variety = 1.0,
    score_biological_biota = 1.007276
  )

  # Should work even with all NA taxonomic levels
  expect_equal(result$score_biological[[1]], 1.007276)
  expect_equal(
    result$candidate_structure_organism_occurrence_closest[[1]],
    "Biota"
  )
})
