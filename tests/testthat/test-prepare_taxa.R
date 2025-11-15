# Test: Prepare Taxa - Taxonomic Assignment
library(testthat)

# =============================================================================
# Tests for prepare_taxa() - Feature Taxonomic Assignment
# =============================================================================

test_that("prepare_taxa validates input parameter", {
  # Test non-character input
  expect_error(
    prepare_taxa(
      input = 123,
      output = "output.tsv"
    ),
    "must be a single character string"
  )

  # Test vector input
  expect_error(
    prepare_taxa(
      input = c("file1.tsv", "file2.tsv"),
      output = "output.tsv"
    ),
    "must be a single character string"
  )
})

test_that("prepare_taxa validates output parameter", {
  temp_input <- tempfile(fileext = ".tsv")
  temp_ott <- tempfile(fileext = ".tsv")

  writeLines("feature_id\tintensity\nFT001\t1000", temp_input)
  writeLines(
    "organism\torganism_taxonomy_ottid\nHomo sapiens\t770309",
    temp_ott
  )

  # Test non-character output
  expect_error(
    prepare_taxa(
      input = temp_input,
      org_tax_ott = temp_ott,
      taxon = "Homo sapiens",
      output = 123
    ),
    "must be a single character string"
  )

  # Test vector output
  expect_error(
    prepare_taxa(
      input = temp_input,
      org_tax_ott = temp_ott,
      taxon = "Homo sapiens",
      output = c("out1.tsv", "out2.tsv")
    ),
    "must be a single character string"
  )

  unlink(temp_input)
  unlink(temp_ott)
})

test_that("prepare_taxa validates org_tax_ott parameter", {
  temp_input <- tempfile(fileext = ".tsv")
  writeLines("feature_id\tintensity\nFT001\t1000", temp_input)

  # Test non-character org_tax_ott
  expect_error(
    prepare_taxa(
      input = temp_input,
      org_tax_ott = 123,
      output = "output.tsv"
    ),
    "must be a single character string"
  )

  # Test vector org_tax_ott
  expect_error(
    prepare_taxa(
      input = temp_input,
      org_tax_ott = c("ott1.tsv", "ott2.tsv"),
      output = "output.tsv"
    ),
    "must be a single character string"
  )

  unlink(temp_input)
})

test_that("prepare_taxa validates extension parameter", {
  temp_input <- tempfile(fileext = ".tsv")
  temp_ott <- tempfile(fileext = ".tsv")

  writeLines("feature_id\tintensity\nFT001\t1000", temp_input)
  writeLines("organism\tottid\nHomo sapiens\t770309", temp_ott)

  # Test non-logical extension
  expect_error(
    prepare_taxa(
      input = temp_input,
      org_tax_ott = temp_ott,
      extension = "yes",
      output = "output.tsv"
    ),
    "must be a single logical value"
  )

  # Test vector extension
  expect_error(
    prepare_taxa(
      input = temp_input,
      org_tax_ott = temp_ott,
      extension = c(TRUE, FALSE),
      output = "output.tsv"
    ),
    "must be a single logical value"
  )

  unlink(temp_input)
  unlink(temp_ott)
})

test_that("prepare_taxa checks input file existence", {
  # Test non-existent input file
  expect_error(
    prepare_taxa(
      input = "nonexistent_features.tsv",
      org_tax_ott = "ott.tsv",
      output = "output.tsv"
    ),
    "not found"
  )
})

test_that("prepare_taxa checks OTT file existence", {
  temp_input <- tempfile(fileext = ".tsv")
  writeLines("feature_id\tintensity\nFT001\t1000", temp_input)

  # Test non-existent OTT file
  expect_error(
    prepare_taxa(
      input = temp_input,
      org_tax_ott = "nonexistent_ott.tsv",
      output = "output.tsv"
    ),
    "not found"
  )

  unlink(temp_input)
})

test_that("prepare_taxa requires metadata if taxon not specified", {
  temp_input <- tempfile(fileext = ".tsv")
  temp_ott <- tempfile(fileext = ".tsv")

  writeLines("feature_id\tintensity\nFT001\t1000", temp_input)
  writeLines("organism\tottid\nHomo sapiens\t770309", temp_ott)

  # Test missing metadata when taxon is NULL
  expect_error(
    prepare_taxa(
      input = temp_input,
      org_tax_ott = temp_ott,
      metadata = "nonexistent_metadata.tsv",
      taxon = NULL,
      output = "output.tsv"
    ),
    "not found|must be provided"
  )

  unlink(temp_input)
  unlink(temp_ott)
})

test_that("prepare_taxa works with single taxon assignment", {
  skip("Integration test - requires full OTT file structure")
  # This test requires proper setup of the OTT taxonomy database
  # which is complex and better tested in integration tests
})

test_that("prepare_taxa handles empty taxon string as NULL", {
  skip("Integration test - requires full OTT file structure and metadata setup")
})

test_that("prepare_taxa works with metadata-based assignment", {
  skip("Integration test - requires full OTT file structure")
})

test_that("prepare_taxa handles pipe-separated organisms", {
  skip("Integration test - requires full OTT file structure")
})
