# Test Suite: prepare_taxa ----

library(testthat)

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

test_that("prepare_taxa works with single taxon assignment", {
  input <- temp_test_path("features.tsv")
  ott <- temp_test_path("ott.tsv")
  output <- temp_test_path("output.tsv")

  # Create minimal valid files
  writeLines("feature_id\trt\tmz\nFT001\t1.5\t200\nFT002\t2.0\t300", input)
  writeLines(
    paste0(
      "organism_name\torganism_taxonomy_ottid\torganism_taxonomy_01domain\t",
      "organism_taxonomy_02kingdom\torganism_taxonomy_03phylum\t",
      "organism_taxonomy_04class\torganism_taxonomy_05order\t",
      "organism_taxonomy_06family\torganism_taxonomy_07tribe\t",
      "organism_taxonomy_08genus\torganism_taxonomy_09species\t",
      "organism_taxonomy_10varietas\n",
      "Homo sapiens\t770309\tEukaryota\tAnimalia\tChordata\t",
      "Mammalia\tPrimates\tHominidae\tNA\tHomo\tHomo sapiens\tNA"
    ),
    ott
  )

  result <- prepare_taxa(
    input = input,
    org_tax_ott = ott,
    taxon = "Homo sapiens",
    output = output,
    extension = FALSE,
    metadata = NULL
  )

  expect_equal(result, output)
  expect_true(file.exists(output))

  # Check output content
  df <- tidytable::fread(output)
  expect_true(all(df$sample_organism_name == "Homo sapiens"))
  expect_equal(nrow(df), 2) # Both features assigned to same organism
})

test_that("prepare_taxa works with metadata-based assignment", {
  input <- temp_test_path("features.tsv")
  ott <- temp_test_path("ott.tsv")
  metadata <- temp_test_path("metadata.tsv")
  output <- temp_test_path("output.tsv")

  # Create features with sample column
  writeLines(
    "feature_id\trt\tmz\tsample\nFT001\t1.5\t200\tfile1\nFT002\t2.0\t300\tfile2",
    input
  )

  # Create OTT taxonomy for two organisms
  writeLines(
    paste0(
      "organism_name\torganism_taxonomy_ottid\torganism_taxonomy_01domain\t",
      "organism_taxonomy_02kingdom\torganism_taxonomy_03phylum\t",
      "organism_taxonomy_04class\torganism_taxonomy_05order\t",
      "organism_taxonomy_06family\torganism_taxonomy_07tribe\t",
      "organism_taxonomy_08genus\torganism_taxonomy_09species\t",
      "organism_taxonomy_10varietas\n",
      "Arabidopsis thaliana\t309275\tEukaryota\tPlantae\tStreptophyta\t",
      "Magnoliopsida\tBrassicales\tBrassicaceae\tNA\tArabidopsis\tArabidopsis thaliana\tNA\n",
      "Oryza sativa\t4530\tEukaryota\tPlantae\tStreptophyta\t",
      "Magnoliopsida\tPoales\tPoaceae\tNA\tOryza\tOryza sativa\tNA"
    ),
    ott
  )

  # Create metadata mapping files to organisms
  writeLines(
    "filename\torganism\nfile1\tArabidopsis thaliana\nfile2\tOryza sativa",
    metadata
  )

  result <- prepare_taxa(
    input = input,
    org_tax_ott = ott,
    taxon = NULL,
    metadata = metadata,
    name_filename = "filename",
    colname = "organism",
    output = output,
    extension = FALSE
  )

  expect_equal(result, output)
  expect_true(file.exists(output))

  # Check output content
  df <- tidytable::fread(output)
  expect_equal(nrow(df), 2)
  expect_true("Arabidopsis thaliana" %in% df$sample_organism_name)
  expect_true("Oryza sativa" %in% df$sample_organism_name)
})

test_that("prepare_taxa handles pipe-separated organisms", {
  input <- temp_test_path("features.tsv")
  ott <- temp_test_path("ott.tsv")
  metadata <- temp_test_path("metadata.tsv")
  output <- temp_test_path("output.tsv")

  writeLines("feature_id\trt\tmz\tsample\nFT001\t1.5\t200\tfile1", input)

  writeLines(
    paste0(
      "organism_name\torganism_taxonomy_ottid\torganism_taxonomy_01domain\t",
      "organism_taxonomy_02kingdom\torganism_taxonomy_03phylum\t",
      "organism_taxonomy_04class\torganism_taxonomy_05order\t",
      "organism_taxonomy_06family\torganism_taxonomy_07tribe\t",
      "organism_taxonomy_08genus\torganism_taxonomy_09species\t",
      "organism_taxonomy_10varietas\n",
      "Org1\t1001\tEukaryota\tPlantae\tNA\tNA\tNA\tNA\tNA\tGenus1\tOrg1\tNA\n",
      "Org2\t1002\tEukaryota\tPlantae\tNA\tNA\tNA\tNA\tNA\tGenus2\tOrg2\tNA"
    ),
    ott
  )

  # Metadata with pipe-separated organisms
  writeLines("filename\torganism\nfile1\tOrg1|Org2", metadata)

  result <- prepare_taxa(
    input = input,
    org_tax_ott = ott,
    taxon = NULL,
    metadata = metadata,
    name_filename = "filename",
    colname = "organism",
    output = output,
    extension = FALSE
  )

  expect_true(file.exists(output))
  df <- tidytable::fread(output)

  # Should have collapsed multiple organisms
  expect_true(grepl("Org1|Org2", df$sample_organism_name[1]))
})

test_that("prepare_taxa works with single taxon assignment", {
  # Create temp files for test
  temp_input <- tempfile(fileext = ".tsv")
  temp_ott <- tempfile(fileext = ".tsv.gz")
  temp_output <- tempfile(fileext = ".tsv.gz")
  temp_metadata <- tempfile(fileext = ".tsv") # Dummy file (won't be used when taxon is specified)

  on.exit({
    unlink(temp_input)
    unlink(temp_ott)
    unlink(temp_output)
    unlink(temp_metadata)
  })

  # Create simple input with feature IDs
  input_data <- data.frame(
    feature_id = c("F1", "F2"),
    intensity = c(100, 200)
  )
  tidytable::fwrite(x = input_data, file = temp_input)

  # Create dummy metadata file (required parameter even though not used with taxon)
  metadata_data <- data.frame(
    filename = c("sample1.mzML"),
    organism = c("Arabidopsis thaliana")
  )
  tidytable::fwrite(x = metadata_data, file = temp_metadata)

  # Create minimal OTT taxonomy
  ott_data <- tidytable::tidytable(
    organism_name = "Arabidopsis thaliana",
    organism_taxonomy_ottid = "309276",
    organism_taxonomy_01domain = "Eukaryota",
    organism_taxonomy_02kingdom = "Archaeplastida",
    organism_taxonomy_03phylum = "Streptophyta",
    organism_taxonomy_04class = "Magnoliopsida",
    organism_taxonomy_05order = "Brassicales",
    organism_taxonomy_06family = "Brassicaceae",
    organism_taxonomy_07tribe = NA_character_,
    organism_taxonomy_08genus = "Arabidopsis",
    organism_taxonomy_09species = "Arabidopsis thaliana",
    organism_taxonomy_10varietas = NA_character_
  )
  export_output(ott_data, temp_ott)

  # Test the function with taxon parameter
  result_path <- prepare_taxa(
    input = temp_input,
    output = temp_output,
    org_tax_ott = temp_ott,
    metadata = temp_metadata, # Provide to avoid default parameter evaluation
    taxon = "Arabidopsis thaliana" # This assigns all features to this organism
  )

  # Verify output exists
  expect_true(file.exists(result_path))

  # Read and verify content
  result <- tidytable::fread(result_path)

  # Should have rows (at least one per feature)
  expect_true(nrow(result) >= 2)

  # All entries should have the same taxonomy
  expect_true(all(result$sample_organism_06_family == "Brassicaceae"))
  expect_true(all(result$sample_organism_08_genus == "Arabidopsis"))
  expect_true(all(result$sample_organism_09_species == "Arabidopsis thaliana"))
})
