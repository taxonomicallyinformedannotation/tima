# ==============================================================================
# Tests for split_tables_sop()
# ==============================================================================
set.seed(NULL)

# Comprehensive test coverage for splitting structure-organism pairs tables
# into normalized component tables.

# ==============================================================================
# Input Validation Tests
# ==============================================================================

test_that("split_tables_sop validates data frame input", {
  # Non-data frame should error
  expect_error(
    split_tables_sop(table = "not a dataframe", cache = NULL),
    "data frame or tibble"
  )

  expect_error(
    split_tables_sop(table = list(a = 1), cache = NULL),
    "data frame or tibble"
  )
})

test_that("split_tables_sop handles empty table gracefully", {
  empty_table <- tidytable::tidytable()

  result <- split_tables_sop(table = empty_table, cache = NULL)

  # Should return list with empty tables
  expect_type(result, "list")
  expect_named(
    result,
    c(
      "table_keys",
      "table_structures_stereo",
      "table_organisms",
      "table_structural"
    )
  )

  # All tables should be empty
  expect_equal(nrow(result$table_keys), 0)
  expect_equal(nrow(result$table_structures_stereo), 0)
  expect_equal(nrow(result$table_organisms), 0)
  expect_equal(nrow(result$table_structural), 0)
})

# ==============================================================================
# Helper Functions
# ==============================================================================

# Create a minimal SOP table with processed SMILES data
# (simulates what process_smiles would return)
create_processed_sop_table <- function(n = 2) {
  tidytable::tidytable(
    structure_inchikey_connectivity_layer = generate_fake_inchikey(
      n,
      seed = 42
    )[1:n] |>
      stringi::stri_sub(1, 14),
    structure_inchikey = generate_fake_inchikey(n, seed = 42),
    structure_smiles = c("CCO", "CCC")[1:n],
    structure_smiles_no_stereo = c("CCO", "CCC")[1:n],
    structure_name = c("Ethanol", "Propane")[1:n],
    structure_molecular_formula = c("C2H6O", "C3H8")[1:n],
    structure_exact_mass = c(46.041865, 44.062600)[1:n],
    structure_xlogp = c(-0.18, 1.09)[1:n],
    organism_name = c("Homo sapiens", "Mus musculus")[1:n],
    organism_taxonomy_01domain = rep("Eukaryota", n),
    organism_taxonomy_02kingdom = rep("Animalia", n),
    organism_taxonomy_03phylum = rep("Chordata", n),
    organism_taxonomy_04class = rep("Mammalia", n),
    organism_taxonomy_05order = c("Primates", "Rodentia")[1:n],
    organism_taxonomy_06family = c("Hominidae", "Muridae")[1:n],
    organism_taxonomy_07tribe = rep(NA_character_, n),
    organism_taxonomy_08genus = c("Homo", "Mus")[1:n],
    organism_taxonomy_09species = c("Homo sapiens", "Mus musculus")[1:n],
    organism_taxonomy_10varietas = rep(NA_character_, n),
    organism_taxonomy_ottid = as.character(123456 + seq_len(n) - 1),
    reference_doi = paste0("10.1234/test", seq_len(n))
  )
}

# ==============================================================================
# Functional Tests
# ==============================================================================

test_that("split_tables_sop splits table into components", {
  skip_on_cran()
  skip("Requires RDKit/Python - process_smiles dependency")

  # Create minimal SOP table with all required columns
  sop_table <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = c(
      "ABCDEFGHIJKLMN",
      "OPQRSTUVWXYZAB"
    ),
    structure_inchikey = c(
      "ABCDEFGHIJKLMN-UHFFFAOYSA-N",
      "OPQRSTUVWXYZAB-UHFFFAOYSA-N"
    ),
    structure_smiles = c("CCO", "CCC"),
    structure_smiles_no_stereo = c("CCO", "CCC"),
    structure_name = c("Ethanol", "Propane"),
    structure_molecular_formula = c("C2H6O", "C3H8"),
    structure_exact_mass = c(46.041865, 44.062600),
    structure_xlogp = c(-0.18, 1.09),
    organism_name = c("Homo sapiens", "Mus musculus"),
    organism_taxonomy_01domain = c("Eukaryota", "Eukaryota"),
    organism_taxonomy_02kingdom = c("Animalia", "Animalia"),
    organism_taxonomy_03phylum = c("Chordata", "Chordata"),
    organism_taxonomy_04class = c("Mammalia", "Mammalia"),
    organism_taxonomy_05order = c("Primates", "Rodentia"),
    organism_taxonomy_06family = c("Hominidae", "Muridae"),
    organism_taxonomy_07tribe = c(NA, NA),
    organism_taxonomy_08genus = c("Homo", "Mus"),
    organism_taxonomy_09species = c("Homo sapiens", "Mus musculus"),
    organism_taxonomy_10varietas = c(NA, NA),
    organism_taxonomy_ottid = c("123456", "234567"),
    reference_doi = c("10.1234/test1", "10.1234/test2")
  )

  result <- split_tables_sop(table = sop_table, cache = NULL)

  # Should return all expected tables
  expect_type(result, "list")
  expect_true(all(
    c(
      "table_keys",
      "table_structures_stereo",
      "table_organisms",
      "table_structural"
    ) %in%
      names(result)
  ))

  # Keys table should have structure-organism pairs
  expect_s3_class(result$table_keys, "data.frame")
  expect_true(
    "structure_inchikey_connectivity_layer" %in% colnames(result$table_keys)
  )

  # Structures table should have stereochemistry info
  expect_s3_class(result$table_structures_stereo, "data.frame")

  # Organisms table should have taxonomy info
  expect_s3_class(result$table_organisms, "data.frame")
  expect_true("organism_name" %in% colnames(result$table_organisms))

  # Structural table should have processed structures
  expect_s3_class(result$table_structural, "data.frame")
})

test_that("split_tables_sop handles missing SMILES gracefully", {
  skip_on_cran()
  skip("Requires RDKit/Python - process_smiles dependency")

  # Table with NA SMILES
  sop_table <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = "ABCDEFGHIJKLMN",
    structure_inchikey = "ABCDEFGHIJKLMN-UHFFFAOYSA-N",
    structure_smiles = NA_character_,
    structure_smiles_no_stereo = "CCO",
    structure_name = "Ethanol",
    structure_molecular_formula = "C2H6O",
    structure_exact_mass = 46.041865,
    structure_xlogp = -0.18,
    organism_name = "Test organism",
    organism_taxonomy_01domain = "Eukaryota",
    organism_taxonomy_02kingdom = "Plantae",
    organism_taxonomy_03phylum = "Tracheophyta",
    organism_taxonomy_04class = "Magnoliopsida",
    organism_taxonomy_05order = "Gentianales",
    organism_taxonomy_06family = "Gentianaceae",
    organism_taxonomy_07tribe = NA,
    organism_taxonomy_08genus = "Gentiana",
    organism_taxonomy_09species = "Gentiana lutea",
    organism_taxonomy_10varietas = NA,
    organism_taxonomy_ottid = "123456",
    reference_doi = "10.1234/test"
  )

  result <- split_tables_sop(table = sop_table, cache = NULL)

  expect_type(result, "list")
  expect_true(nrow(result$table_keys) >= 0)
})

test_that("split_tables_sop preserves unique organisms", {
  skip_on_cran()
  skip("Requires RDKit/Python - process_smiles dependency")

  # Table with duplicate organisms
  sop_table <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = c(
      "ABCDEFGHIJKLMN",
      "OPQRSTUVWXYZAB"
    ),
    structure_inchikey = c(
      "ABCDEFGHIJKLMN-UHFFFAOYSA-N",
      "OPQRSTUVWXYZAB-UHFFFAOYSA-N"
    ),
    structure_smiles = c("CCO", "CCC"),
    structure_smiles_no_stereo = c("CCO", "CCC"),
    structure_name = c("Ethanol", "Propane"),
    structure_molecular_formula = c("C2H6O", "C3H8"),
    structure_exact_mass = c(46.041865, 44.062600),
    structure_xlogp = c(-0.18, 1.09),
    organism_name = c("Homo sapiens", "Homo sapiens"), # Same organism
    organism_taxonomy_01domain = c("Eukaryota", "Eukaryota"),
    organism_taxonomy_02kingdom = c("Animalia", "Animalia"),
    organism_taxonomy_03phylum = c("Chordata", "Chordata"),
    organism_taxonomy_04class = c("Mammalia", "Mammalia"),
    organism_taxonomy_05order = c("Primates", "Primates"),
    organism_taxonomy_06family = c("Hominidae", "Hominidae"),
    organism_taxonomy_07tribe = c(NA, NA),
    organism_taxonomy_08genus = c("Homo", "Homo"),
    organism_taxonomy_09species = c("Homo sapiens", "Homo sapiens"),
    organism_taxonomy_10varietas = c(NA, NA),
    organism_taxonomy_ottid = c("123456", "123456"),
    reference_doi = c("10.1234/test1", "10.1234/test2")
  )

  result <- split_tables_sop(table = sop_table, cache = NULL)

  # Organisms table should deduplicate
  expect_s3_class(result$table_organisms, "data.frame")
  # Should have only 1 unique organism
  unique_orgs <- result$table_organisms |>
    tidytable::distinct(organism_name) |>
    nrow()
  expect_equal(unique_orgs, 1)
})

test_that("split_tables_sop handles cache parameter", {
  skip_on_cran()
  skip("Requires RDKit/Python - process_smiles dependency")

  sop_table <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = "ABCDEFGHIJKLMN",
    structure_inchikey = "ABCDEFGHIJKLMN-UHFFFAOYSA-N",
    structure_smiles = "CCO",
    structure_smiles_no_stereo = "CCO",
    structure_name = "Ethanol",
    structure_molecular_formula = "C2H6O",
    structure_exact_mass = 46.041865,
    structure_xlogp = -0.18,
    organism_name = "Test organism",
    organism_taxonomy_01domain = "Eukaryota",
    organism_taxonomy_02kingdom = "Plantae",
    organism_taxonomy_03phylum = "Tracheophyta",
    organism_taxonomy_04class = "Magnoliopsida",
    organism_taxonomy_05order = "Gentianales",
    organism_taxonomy_06family = "Gentianaceae",
    organism_taxonomy_07tribe = NA,
    organism_taxonomy_08genus = "Gentiana",
    organism_taxonomy_09species = "Gentiana lutea",
    organism_taxonomy_10varietas = NA,
    organism_taxonomy_ottid = "123456",
    reference_doi = "10.1234/test"
  )

  # With NULL cache
  result_no_cache <- split_tables_sop(table = sop_table, cache = NULL)
  expect_type(result_no_cache, "list")

  # With cache path (file doesn't need to exist)
  withr::local_tempdir()
  cache_file <- file.path(tempdir(), "test_cache.tsv")
  result_with_cache <- split_tables_sop(table = sop_table, cache = cache_file)
  expect_type(result_with_cache, "list")
})

# ==============================================================================
# Edge Cases
# ==============================================================================

test_that("split_tables_sop handles special characters in organism names", {
  skip_on_cran()
  skip("Requires RDKit/Python - process_smiles dependency")

  sop_table <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = "ABCDEFGHIJKLMN",
    structure_inchikey = "ABCDEFGHIJKLMN-UHFFFAOYSA-N",
    structure_smiles = "CCO",
    structure_smiles_no_stereo = "CCO",
    structure_name = "Ethanol",
    structure_molecular_formula = "C2H6O",
    structure_exact_mass = 46.041865,
    structure_xlogp = -0.18,
    organism_name = "Test-organism (with special) chars!",
    organism_taxonomy_01domain = "Eukaryota",
    organism_taxonomy_02kingdom = "Plantae",
    organism_taxonomy_03phylum = "Tracheophyta",
    organism_taxonomy_04class = "Magnoliopsida",
    organism_taxonomy_05order = "Gentianales",
    organism_taxonomy_06family = "Gentianaceae",
    organism_taxonomy_07tribe = NA,
    organism_taxonomy_08genus = "Gentiana",
    organism_taxonomy_09species = "Gentiana lutea",
    organism_taxonomy_10varietas = NA,
    organism_taxonomy_ottid = "123456",
    reference_doi = "10.1234/test"
  )

  result <- split_tables_sop(table = sop_table, cache = NULL)

  expect_type(result, "list")
  expect_true(nrow(result$table_organisms) > 0)
})

test_that("split_tables_sop handles missing taxonomy fields", {
  skip_on_cran()
  skip("Requires RDKit/Python - process_smiles dependency")

  sop_table <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = "ABCDEFGHIJKLMN",
    structure_inchikey = "ABCDEFGHIJKLMN-UHFFFAOYSA-N",
    structure_smiles = "CCO",
    structure_smiles_no_stereo = "CCO",
    structure_name = "Ethanol",
    structure_molecular_formula = "C2H6O",
    structure_exact_mass = 46.041865,
    structure_xlogp = -0.18,
    organism_name = "Test organism",
    organism_taxonomy_01domain = NA,
    organism_taxonomy_02kingdom = NA,
    organism_taxonomy_03phylum = NA,
    organism_taxonomy_04class = NA,
    organism_taxonomy_05order = NA,
    organism_taxonomy_06family = NA,
    organism_taxonomy_07tribe = NA,
    organism_taxonomy_08genus = NA,
    organism_taxonomy_09species = NA,
    organism_taxonomy_10varietas = NA,
    organism_taxonomy_ottid = "123456",
    reference_doi = "10.1234/test"
  )

  result <- split_tables_sop(table = sop_table, cache = NULL)

  expect_type(result, "list")
  expect_true(nrow(result$table_organisms) >= 0)
})
