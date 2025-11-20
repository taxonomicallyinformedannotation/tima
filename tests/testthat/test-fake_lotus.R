# Test Suite: fake_lotus ----

library(testthat)

test_that("fake_lotus creates file with correct structure", {
  skip_on_cran()

  temp_file <- tempfile(fileext = ".tsv.gz")

  result <- fake_lotus(export = temp_file)

  expect_equal(result, temp_file)
  expect_true(file.exists(temp_file))

  # Read and verify structure
  data <- tidytable::fread(temp_file)

  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 0)

  # Verify all expected columns exist
  expected_cols <- c(
    "structure_wikidata",
    "structure_inchikey",
    "structure_inchi",
    "structure_smiles",
    "structure_molecular_formula",
    "structure_exact_mass",
    "structure_xlogp",
    "structure_smiles_2D",
    "structure_cid",
    "structure_nameIupac",
    "structure_nameTraditional",
    "structure_stereocenters_total",
    "structure_stereocenters_unspecified",
    "structure_taxonomy_npclassifier_01pathway",
    "structure_taxonomy_npclassifier_02superclass",
    "structure_taxonomy_npclassifier_03class",
    "structure_taxonomy_classyfire_chemontid",
    "structure_taxonomy_classyfire_01kingdom",
    "structure_taxonomy_classyfire_02superclass",
    "structure_taxonomy_classyfire_03class",
    "structure_taxonomy_classyfire_04directparent",
    "organism_wikidata",
    "organism_name",
    "organism_taxonomy_gbifid",
    "organism_taxonomy_ncbiid",
    "organism_taxonomy_ottid",
    "organism_taxonomy_01domain",
    "organism_taxonomy_02kingdom",
    "organism_taxonomy_03phylum",
    "organism_taxonomy_04class",
    "organism_taxonomy_05order",
    "organism_taxonomy_06family",
    "organism_taxonomy_07tribe",
    "organism_taxonomy_08genus",
    "organism_taxonomy_09species",
    "organism_taxonomy_10varietas",
    "reference_wikidata",
    "reference_doi",
    "manual_validation"
  )

  expect_true(all(expected_cols %in% names(data)))
})

test_that("fake_lotus validates input parameters", {
  skip_on_cran()

  # Missing export parameter
  expect_error(fake_lotus(), "export path must be a single character string")

  # NULL export
  expect_error(
    fake_lotus(export = NULL),
    "export path must be a single character string"
  )

  # Non-character export
  expect_error(
    fake_lotus(export = 123),
    "export path must be a single character string"
  )

  # Multiple paths
  expect_error(
    fake_lotus(export = c("path1", "path2")),
    "export path must be a single character string"
  )
})

test_that("fake_lotus creates directory if needed", {
  skip_on_cran()

  temp_dir <- tempfile()
  temp_file <- file.path(temp_dir, "subfolder", "lotus.tsv.gz")

  expect_false(dir.exists(temp_dir))

  fake_lotus(export = temp_file)

  expect_true(file.exists(temp_file))
  expect_true(dir.exists(dirname(temp_file)))
})

test_that("fake_lotus overwrites existing file", {
  skip_on_cran()

  temp_file <- tempfile(fileext = ".tsv.gz")

  # Create first version
  fake_lotus(export = temp_file)
  first_time <- file.info(temp_file)$mtime

  Sys.sleep(0.1)

  # Overwrite
  fake_lotus(export = temp_file)
  second_time <- file.info(temp_file)$mtime

  expect_true(second_time >= first_time)
  expect_true(file.exists(temp_file))
})
