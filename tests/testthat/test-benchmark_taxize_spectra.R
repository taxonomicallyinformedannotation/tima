# Test Suite: benchmark_taxize_spectra ----

library(testthat)

## Validation ----

test_that("test-benchmark_taxize_spectra validates input file exists", {
  tmp <- withr::local_tempdir()
  expect_error(
    benchmark_taxize_spectra(
      input = file.path(tmp, "missing.tsv"),
      keys = file.path(tmp, "keys.tsv"),
      org_tax_ott = file.path(tmp, "tax.tsv"),
      output = file.path(tmp, "out.tsv")
    ),
    "Input features file not found"
  )
})

test_that("test-benchmark_taxize_spectra validates keys file exists", {
  tmp <- withr::local_tempdir()
  input <- file.path(tmp, "input.tsv")
  writeLines("feature_id", input)

  expect_error(
    benchmark_taxize_spectra(
      input = input,
      keys = file.path(tmp, "missing.tsv"),
      org_tax_ott = file.path(tmp, "tax.tsv"),
      output = file.path(tmp, "out.tsv")
    ),
    "Keys file not found"
  )
})

test_that("test-benchmark_taxize_spectra validates taxonomy file exists", {
  tmp <- withr::local_tempdir()
  input <- file.path(tmp, "input.tsv")
  keys <- file.path(tmp, "keys.tsv")
  writeLines("feature_id", input)
  writeLines("structure_inchikey", keys)

  expect_error(
    benchmark_taxize_spectra(
      input = input,
      keys = keys,
      org_tax_ott = file.path(tmp, "missing.tsv"),
      output = file.path(tmp, "out.tsv")
    ),
    "Taxonomy file not found"
  )
})

## Behavior ----

test_that("test-benchmark_taxize_spectra processes minimal valid input", {
  tmp <- withr::local_tempdir()

  # Create minimal features file
  features <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    inchikey_connectivity_layer = c("AAAAAAAAAAAAAA", "BBBBBBBBBBBBBB")
  )
  input <- file.path(tmp, "features.tsv")
  tidytable::fwrite(features, input, sep = "\t")

  # Create minimal SOP keys
  sop <- tidytable::tidytable(
    structure_inchikey = c("AAAAAAAAAAAAAA-XXXXXXXX-X"),
    organism_name = c("Test organism")
  )
  keys <- file.path(tmp, "keys.tsv")
  tidytable::fwrite(sop, keys, sep = "\t")

  # Create minimal taxonomy with all required columns
  tax <- tidytable::tidytable(
    organism_name = c("Test organism"),
    organism_taxonomy_01domain = c("Eukaryota"),
    organism_taxonomy_02kingdom = c("Plantae"),
    organism_taxonomy_03phylum = c("TestPhylum"),
    organism_taxonomy_04class = c("TestClass"),
    organism_taxonomy_05order = c("TestOrder"),
    organism_taxonomy_06family = c("TestFamily"),
    organism_taxonomy_07tribe = c("TestTribe"),
    organism_taxonomy_08genus = c("TestGenus"),
    organism_taxonomy_09species = c("Test organism"),
    organism_taxonomy_10varietas = c(NA_character_)
  )
  org_tax_ott <- file.path(tmp, "tax.tsv")
  tidytable::fwrite(tax, org_tax_ott, sep = "\t")

  output <- file.path(tmp, "output.tsv")

  res <- benchmark_taxize_spectra(
    input = input,
    keys = keys,
    org_tax_ott = org_tax_ott,
    output = output
  )

  expect_equal(res, output)
  expect_true(file.exists(output))
  result <- tidytable::fread(output)
  expect_true("feature_id" %in% names(result))
})

test_that("test-benchmark_taxize_spectra handles features without organisms", {
  tmp <- withr::local_tempdir()

  features <- tidytable::tidytable(
    feature_id = c("F1"),
    inchikey_connectivity_layer = c("ZZZZZZZZZZZZZZ")
  )
  input <- file.path(tmp, "features.tsv")
  tidytable::fwrite(features, input, sep = "\t")

  # Empty keys
  sop <- tidytable::tidytable(
    structure_inchikey = character(),
    organism_name = character()
  )
  keys <- file.path(tmp, "keys.tsv")
  tidytable::fwrite(sop, keys, sep = "\t")

  tax <- tidytable::tidytable(
    organism_name = character(),
    organism_taxonomy_01domain = character(),
    organism_taxonomy_02kingdom = character(),
    organism_taxonomy_03phylum = character(),
    organism_taxonomy_04class = character(),
    organism_taxonomy_05order = character(),
    organism_taxonomy_06family = character(),
    organism_taxonomy_07tribe = character(),
    organism_taxonomy_08genus = character(),
    organism_taxonomy_09species = character(),
    organism_taxonomy_10varietas = character()
  )
  org_tax_ott <- file.path(tmp, "tax.tsv")
  tidytable::fwrite(tax, org_tax_ott, sep = "\t")

  output <- file.path(tmp, "output.tsv")

  res <- benchmark_taxize_spectra(
    input = input,
    keys = keys,
    org_tax_ott = org_tax_ott,
    output = output
  )

  expect_true(file.exists(output))
  result <- tidytable::fread(output)
  expect_equal(nrow(result), 1)
})
