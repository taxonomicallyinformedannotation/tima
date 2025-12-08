# Test Suite: prepare_libraries_sop_bigg ----

library(testthat)

## Simplified test models for unit tests
test_bigg_models <- list(
  "Escherichia coli" = c(
    "model_id" = "e_coli_core",
    "doi" = "10.1234/5678"
  )
)

## Validation ----

test_that("prepare_libraries_sop_bigg validates output path parameter", {
  # Non-character output
  expect_error(
    prepare_libraries_sop_bigg(
      bigg_models = test_bigg_models,
      output = 123
    ),
    "output must be a single character string"
  )

  # Vector output
  expect_error(
    prepare_libraries_sop_bigg(
      bigg_models = test_bigg_models,
      output = c("out1.tsv", "out2.tsv")
    ),
    "output must be a single character string"
  )

  # NULL output
  expect_error(
    prepare_libraries_sop_bigg(
      bigg_models = test_bigg_models,
      output = NULL
    ),
    "output must be a single character string"
  )
})

test_that("prepare_libraries_sop_bigg validates output is single value", {
  expect_error(
    prepare_libraries_sop_bigg(
      bigg_models = test_bigg_models,
      output = character(0)
    ),
    "output must be a single character string"
  )
})

## Behavior ----

test_that("prepare_libraries_sop_bigg uses existing valid output file", {
  output_file <- temp_test_path("bigg_output_existing.tsv")

  # Create a valid existing output file (> 100000 bytes)
  mock_data <- tidytable::tidytable(
    structure_inchikey = rep("AAAABBBBCCCCDD-EEEEFFFFF-G", 5000),
    structure_inchikey_2D = rep("AAAABBBBCCCCDD", 5000),
    structure_smiles_2D = rep("C1=CC=CC=C1", 5000),
    structure_name = rep("benzene", 5000),
    organism_name = rep("Escherichia coli", 5000),
    organism_taxonomy_ottid = rep(474506L, 5000),
    reference_doi = rep("10.1234/5678", 5000)
  )
  tidytable::fwrite(x = mock_data, file = output_file, sep = "\t")

  initial_size <- file.size(output_file)
  initial_mtime <- file.mtime(output_file)

  # Run function
  Sys.sleep(0.1)  # Small delay to ensure mtime difference if file is rewritten
  result <- prepare_libraries_sop_bigg(
    bigg_models = test_bigg_models,
    output = output_file
  )

  # Should return existing file without regenerating
  expect_equal(result, output_file)
  expect_equal(file.size(output_file), initial_size)
  expect_equal(file.mtime(output_file), initial_mtime)
})

test_that("prepare_libraries_sop_bigg just runs", {
  output_file <- temp_test_path("bigg_output.tsv")

  # Create a very small invalid file
  writeLines("invalid\ndata", output_file)
  small_size <- file.size(output_file)

  expect_true(small_size < 100000)

  result <- prepare_libraries_sop_bigg(
    bigg_models = test_bigg_models,
    output = output_file
  )

  df <- tidytable::fread(result)
  
  # Check for expected columns
  expect_true("structure_inchikey" %in% names(df))
  expect_true("structure_name" %in% names(df))
  expect_true("organism_name" %in% names(df))
  
  # Check for organism taxonomy columns
  expect_true("organism_taxonomy_ottid" %in% names(df))
  expect_true("organism_taxonomy_01domain" %in% names(df))
  expect_true("organism_taxonomy_09species" %in% names(df))
  expect_true("reference_doi" %in% names(df))
  
  raw_content <- readLines(result, n = 2)
  expect_true(any(grepl("\t", raw_content, fixed = TRUE)))
  
  expect_true(file.exists(result))
  expect_equal(result, output_file)
  expect_type(result, "character")
  
  expect_length(result, 1L)
  expect_false(is.na(result))
  expect_false(result == "")
})
