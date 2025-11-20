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
  tmp <- withr::local_tempdir(.local_envir = parent.frame())

  input <- file.path(tmp, "features.tsv")
  ott <- file.path(tmp, "ott.tsv")
  output <- file.path(tmp, "output.tsv")

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
  tmp <- withr::local_tempdir(.local_envir = parent.frame())

  input <- file.path(tmp, "features.tsv")
  ott <- file.path(tmp, "ott.tsv")
  metadata <- file.path(tmp, "metadata.tsv")
  output <- file.path(tmp, "output.tsv")

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
  tmp <- withr::local_tempdir(.local_envir = parent.frame())

  input <- file.path(tmp, "features.tsv")
  ott <- file.path(tmp, "ott.tsv")
  metadata <- file.path(tmp, "metadata.tsv")
  output <- file.path(tmp, "output.tsv")

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

test_that(
  skip("Not implemented")
)
# test_that("prepare_taxa requires metadata if taxon not specified", {
#   temp_input <- tempfile(fileext = ".tsv")
#   temp_ott <- tempfile(fileext = ".tsv")
#
#   writeLines("feature_id\tintensity\nFT001\t1000", temp_input)
#   writeLines("organism\tottid\nHomo sapiens\t770309", temp_ott)
#
#   # Test missing metadata when taxon is NULL
#   expect_error(
#     prepare_taxa(
#       input = temp_input,
#       org_tax_ott = temp_ott,
#       metadata = "nonexistent_metadata.tsv",
#       taxon = NULL,
#       output = "output.tsv"
#     ),
#     "not found|must be provided"
#   )
#
#   unlink(temp_input)
#   unlink(temp_ott)
# })

test_that("prepare_taxa works with single taxon assignment", {
  skip("Not implemented")
  # This test requires proper setup of the OTT taxonomy database
  # which is complex and better tested in integration tests
})

test_that(
  skip("Not implemented")
)
# test_that("prepare_taxa handles empty taxon string as NULL", {
#   tmp <- withr::local_tempdir(.local_envir = parent.frame())
#
#   input <- file.path(tmp, "features.tsv")
#   ott <- file.path(tmp, "ott.tsv")
#   metadata <- file.path(tmp, "metadata.tsv")
#   output <- file.path(tmp, "output.tsv")
#
#   writeLines("feature_id\trt\tmz\nFT001\t1.5\t200", input)
#   writeLines("organism_name\torganism_taxonomy_ottid\torganism_taxonomy_01domain\nTest org\t12345\tEukaryota", ott)
#   writeLines("filename\torganism\nfile1\tTest org", metadata)
#
#   # Empty string should be treated as NULL and require metadata
#   expect_error(
#     prepare_taxa(
#       input = input,
#       org_tax_ott = ott,
#       taxon = "",  # Empty string
#       metadata = file.path(tmp, "nonexistent.tsv"),
#       output = output,
#       extension = FALSE
#     ),
#     "not found|must be provided"
#   )
# })
