# Test Suite: weight_bio ----
# Tests the biological weighting step. Uses shared helpers to avoid
# repeating 25-line SOP builder boilerplate in every test.

library(testthat)

# ---- shared helpers ----------------------------------------------------------

# Default weight_bio score parameters (avoid repeating 11 args everywhere)
DEFAULT_BIO_SCORES <- list(
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

# Build a minimal SOP row for the given inchikey and organism taxonomy
sop_for <- function(
  inchikey,
  organism = "Gentiana lutea",
  ottid = "123456",
  domain = "Eukaryota",
  kingdom = "Plantae",
  phylum = "Tracheophyta",
  class_tax = "Magnoliopsida",
  order_tax = "Gentianales",
  family = "Gentianaceae",
  tribe = NA_character_,
  genus = "Gentiana",
  species = "Gentiana lutea",
  varietas = NA_character_
) {
  tidytable::tidytable(
    structure_inchikey_connectivity_layer = inchikey,
    organism_name = organism,
    organism_taxonomy_ottid = ottid,
    organism_taxonomy_01domain = domain,
    organism_taxonomy_02kingdom = kingdom,
    organism_taxonomy_03phylum = phylum,
    organism_taxonomy_04class = class_tax,
    organism_taxonomy_05order = order_tax,
    organism_taxonomy_06family = family,
    organism_taxonomy_07tribe = tribe,
    organism_taxonomy_08genus = genus,
    organism_taxonomy_09species = species,
    organism_taxonomy_10varietas = varietas
  )
}

# Thin wrapper that calls weight_bio with default scores
call_wb <- function(annotations, sop, ...) {
  do.call(
    weight_bio,
    c(
      list(
        annotation_table_taxed = annotations,
        structure_organism_pairs_table = sop
      ),
      DEFAULT_BIO_SCORES,
      list(...)
    )
  )
}

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

# ---- input validation --------------------------------------------------------

test_that("weight_bio rejects non-data-frame annotation_table_taxed", {
  expect_error(
    call_wb("not_a_df", sop_for("IK1")),
    "data frame"
  )
})

test_that("weight_bio rejects out-of-range score parameters", {
  ann <- create_test_annotation("F1", "IK1")
  sop <- sop_for("IK1")
  expect_error(
    do.call(
      weight_bio,
      c(
        list(
          annotation_table_taxed = ann,
          structure_organism_pairs_table = sop
        ),
        modifyList(DEFAULT_BIO_SCORES, list(score_biological_domain = -0.5))
      )
    ),
    "between 0 and 1"
  )
})

# ---- empty input -------------------------------------------------------------

test_that("weight_bio returns 0-row data frame for empty annotation table", {
  empty <- create_test_annotation(character(0), character(0))
  result <- call_wb(empty, sop_for("IK1"))
  expect_equal(nrow(result), 0L)
  expect_s3_class(result, "data.frame")
})

# ---- taxonomic score hierarchy -----------------------------------------------

test_that("weight_bio assigns species-level score for exact species match", {
  ik <- "IK_SPECIES"
  ann <- create_test_annotation("F1", ik, species = "Gentiana lutea")
  sop <- sop_for(ik, species = "Gentiana lutea")
  out <- call_wb(ann, sop)
  expect_equal(out$score_biological[[1L]], 0.9)
})

test_that("weight_bio assigns genus-level score for same-genus different-species", {
  ik <- "IK_GENUS"
  ann <- create_test_annotation(
    "F1",
    ik,
    species = "Gentiana lutea",
    genus = "Gentiana"
  )
  sop <- sop_for(ik, species = "Gentiana acaulis", genus = "Gentiana")
  out <- call_wb(ann, sop)
  expect_equal(out$score_biological[[1L]], 0.8)
})

test_that("weight_bio assigns family-level score for same-family different-genus", {
  ik <- "IK_FAM"
  ann <- create_test_annotation(
    "F1",
    ik,
    genus = "Gentiana",
    family = "Gentianaceae"
  )
  sop <- sop_for(
    ik,
    genus = "Swertia",
    species = "Swertia perennis",
    family = "Gentianaceae"
  )
  out <- call_wb(ann, sop)
  expect_equal(out$score_biological[[1L]], 0.6)
})

test_that("weight_bio assigns 0 for unmatched structures", {
  ann <- create_test_annotation(c("F1", "F2"), c("IK_A", "IK_B"))
  sop <- sop_for("IK_DIFFERENT")
  out <- call_wb(ann, sop)
  expect_true(all(out$score_biological %in% c(0, NA_real_)))
})

# ---- Biota domain --------------------------------------------------------

test_that("weight_bio gives max score (biota) to Biota domain candidates", {
  ik <- "BDAGIHXWWSANSR"
  ann <- create_test_annotation(
    "F1",
    ik,
    domain = "Eukaryota",
    kingdom = "Metazoa"
  )
  sop <- sop_for(
    ik,
    organism = "Biota",
    ottid = "0",
    domain = "Biota",
    kingdom = NA_character_,
    phylum = NA_character_,
    class_tax = NA_character_,
    order_tax = NA_character_,
    family = NA_character_,
    genus = NA_character_,
    species = NA_character_
  )
  out <- call_wb(ann, sop)
  expect_equal(out$score_biological[[1L]], 1.007276)
  expect_equal(
    out$candidate_structure_organism_occurrence_closest[[1L]],
    "Biota"
  )
})

test_that("Biota score beats exact species match", {
  ik <- "SHARED_IK"
  ann <- create_test_annotation(c("F1", "F2"), rep(ik, 2))
  sop <- tidytable::bind_rows(
    sop_for(
      ik,
      organism = "Biota",
      ottid = "0",
      domain = "Biota",
      kingdom = NA_character_,
      phylum = NA_character_,
      class_tax = NA_character_,
      order_tax = NA_character_,
      family = NA_character_,
      genus = NA_character_,
      species = NA_character_
    ),
    sop_for(ik) # exact match
  )
  out <- call_wb(ann, sop)
  expect_true(max(out$score_biological, na.rm = TRUE) == 1.007276)
})

# ---- output columns ----------------------------------------------------------

test_that("weight_bio result contains score_biological and score_weighted_bio", {
  ik <- "IK_OUT"
  out <- call_wb(
    create_test_annotation("F1", ik),
    sop_for(ik)
  )
  expect_true("score_biological" %in% names(out))
  expect_true("score_weighted_bio" %in% names(out))
  expect_true("feature_id" %in% names(out))
})
