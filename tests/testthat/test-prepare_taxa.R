# Test Suite: prepare_taxa ----

library(testthat)

test_that("prepare_taxa validates input parameter", {
  # Test non-character input
  expect_error(
    prepare_taxa(
      input = 123,
      output = "output.tsv"
    ),
    "Fix: Ensure the parameter is a length-1 character value",
    fixed = TRUE
  )

  # Test vector input
  expect_error(
    prepare_taxa(
      input = c("file1.tsv", "file2.tsv"),
      output = "output.tsv"
    ),
    "Fix: Ensure the parameter is a length-1 character value",
    fixed = TRUE
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
    "must be a single character string",
    class = "tima_validation_error"
  )

  # Test vector output
  expect_error(
    prepare_taxa(
      input = temp_input,
      org_tax_ott = temp_ott,
      taxon = "Homo sapiens",
      output = c("out1.tsv", "out2.tsv")
    ),
    "must be a single character string",
    class = "tima_validation_error"
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
    "Fix: Ensure the parameter is a length-1 character value",
    fixed = TRUE
  )

  # Test vector org_tax_ott
  expect_error(
    prepare_taxa(
      input = temp_input,
      org_tax_ott = c("ott1.tsv", "ott2.tsv"),
      output = "output.tsv"
    ),
    "Fix: Ensure the parameter is a length-1 character value",
    fixed = TRUE
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
    "must be a single logical value",
    class = "tima_validation_error"
  )

  # Test vector extension
  expect_error(
    prepare_taxa(
      input = temp_input,
      org_tax_ott = temp_ott,
      extension = c(TRUE, FALSE),
      output = "output.tsv"
    ),
    "must be a single logical value",
    class = "tima_validation_error"
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

test_that("prepare_taxa works with multiple taxon assignment", {
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
      "Homo sapiens $ Arabidopsis thaliana\t770309 $ 309275\tEukaryota\t",
      "Animalia $ Plantae\tChordata $ Streptophyta\tMammalia $ Magnoliopsida\t",
      "Primates $ Brassicales\tHominidae $ Brassicaceae\tNA\tHomo $Arabidopsis\t",
      "Homo sapiens $ Arabidopsis thaliana\tNA"
    ),
    ott
  )

  result <- prepare_taxa(
    input = input,
    org_tax_ott = ott,
    taxon = "Homo sapiens|Arabidopsis thaliana",
    output = output,
    extension = FALSE,
    metadata = NULL
  )

  expect_equal(result, output)
  expect_true(file.exists(output))

  # Check output content
  df <- tidytable::fread(output)
  expect_true(all(
    df$sample_organism_name == "Arabidopsis thaliana $ Homo sapiens"
  ))
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

test_that("prepare_taxa falls back to ATTRIBUTE_species when requested taxon column is missing", {
  input <- temp_test_path("features.tsv")
  ott <- temp_test_path("ott.tsv")
  metadata <- temp_test_path("metadata.tsv")
  output <- temp_test_path("output.tsv")

  writeLines(
    "feature_id\trt\tmz\tsample\nFT001\t1.5\t200\tfile1\nFT002\t2.0\t300\tfile2",
    input
  )

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

  # No 'organism' column on purpose, only ATTRIBUTE_species.
  writeLines(
    "filename\tATTRIBUTE_species\nfile1\tArabidopsis thaliana\nfile2\tOryza sativa",
    metadata
  )

  expect_no_error(
    prepare_taxa(
      input = input,
      org_tax_ott = ott,
      taxon = NULL,
      metadata = metadata,
      name_filename = "filename",
      colname = "organism",
      output = output,
      extension = FALSE
    )
  )

  df <- tidytable::fread(output)
  expect_equal(nrow(df), 2L)
  expect_true("Arabidopsis thaliana" %in% df$sample_organism_name)
  expect_true("Oryza sativa" %in% df$sample_organism_name)
})

test_that("prepare_taxa exports ND taxonomy when metadata lacks organism values", {
  input <- temp_test_path("features.tsv")
  ott <- temp_test_path("ott.tsv")
  metadata <- temp_test_path("metadata.tsv")
  output <- temp_test_path("output.tsv")

  writeLines(
    "feature_id\trt\tmz\tsample\nFT001\t1.5\t200\tfile1\nFT002\t2.0\t300\tfile2",
    input
  )

  # OTT file exists (validated early), but should not be required for ND fallback.
  writeLines(
    paste0(
      "organism_name\torganism_taxonomy_ottid\torganism_taxonomy_01domain\t",
      "organism_taxonomy_02kingdom\torganism_taxonomy_03phylum\t",
      "organism_taxonomy_04class\torganism_taxonomy_05order\t",
      "organism_taxonomy_06family\torganism_taxonomy_07tribe\t",
      "organism_taxonomy_08genus\torganism_taxonomy_09species\t",
      "organism_taxonomy_10varietas\n",
      "dummy\t1\tNA\tNA\tNA\tNA\tNA\tNA\tNA\tNA\tNA\tNA"
    ),
    ott
  )

  # No organism-like columns at all.
  writeLines("filename\tbatch\nfile1\tA\nfile2\tB", metadata)

  expect_no_error(
    prepare_taxa(
      input = input,
      org_tax_ott = ott,
      taxon = NULL,
      metadata = metadata,
      name_filename = "filename",
      colname = "organism",
      output = output,
      extension = FALSE
    )
  )

  df <- tidytable::fread(output)
  expect_equal(nrow(df), 2L)
  expect_true(all(df$sample_organism_name == "ND"))
  expect_true(all(df$sample_organism_01_domain == "ND"))
})

test_that("prepare_taxa treats empty taxon string as metadata mode", {
  input <- temp_test_path("features-empty-taxon.tsv")
  ott <- temp_test_path("ott-empty-taxon.tsv")
  metadata <- temp_test_path("metadata-empty-taxon.tsv")
  output <- temp_test_path("output-empty-taxon.tsv")

  writeLines(
    "feature_id\trt\tmz\tsample\nFT001\t1.5\t200\tfile1",
    input
  )

  writeLines(
    paste0(
      "organism_name\torganism_taxonomy_ottid\torganism_taxonomy_01domain\t",
      "organism_taxonomy_02kingdom\torganism_taxonomy_03phylum\t",
      "organism_taxonomy_04class\torganism_taxonomy_05order\t",
      "organism_taxonomy_06family\torganism_taxonomy_07tribe\t",
      "organism_taxonomy_08genus\torganism_taxonomy_09species\t",
      "organism_taxonomy_10varietas\n",
      "Arabidopsis thaliana\t309275\tEukaryota\tPlantae\tStreptophyta\t",
      "Magnoliopsida\tBrassicales\tBrassicaceae\tNA\tArabidopsis\tArabidopsis thaliana\tNA"
    ),
    ott
  )

  writeLines("filename\torganism\nfile1\tArabidopsis thaliana", metadata)

  expect_no_error(
    prepare_taxa(
      input = input,
      org_tax_ott = ott,
      taxon = "",
      metadata = metadata,
      name_filename = "filename",
      colname = "organism",
      output = output,
      extension = FALSE
    )
  )

  df <- tidytable::fread(output)
  expect_equal(nrow(df), 1L)
  expect_identical(df$sample_organism_name[[1]], "Arabidopsis thaliana")
})

test_that("prepare_taxa errors when OTT table misses required schema", {
  input <- temp_test_path("features-bad-ott.tsv")
  ott <- temp_test_path("ott-bad-schema.tsv")

  writeLines("feature_id\trt\tmz\nFT001\t1.5\t200", input)
  writeLines(
    "organism_name\torganism_taxonomy_ottid\nHomo sapiens\t770309",
    ott
  )

  expect_error(
    prepare_taxa(
      input = input,
      org_tax_ott = ott,
      taxon = "Homo sapiens",
      output = temp_test_path("out-bad-ott.tsv")
    ),
    "missing required columns",
    class = "tima_validation_error"
  )
})

test_that("prepare_taxa errors when features sample column is missing", {
  input <- temp_test_path("features-no-sample.tsv")
  ott <- temp_test_path("ott-no-sample.tsv")
  metadata <- temp_test_path("metadata-no-sample.tsv")

  writeLines("feature_id\trt\tmz\nFT001\t1.5\t200", input)
  writeLines(
    paste0(
      "organism_name\torganism_taxonomy_ottid\torganism_taxonomy_01domain\t",
      "organism_taxonomy_02kingdom\torganism_taxonomy_03phylum\t",
      "organism_taxonomy_04class\torganism_taxonomy_05order\t",
      "organism_taxonomy_06family\torganism_taxonomy_07tribe\t",
      "organism_taxonomy_08genus\torganism_taxonomy_09species\t",
      "organism_taxonomy_10varietas\n",
      "Arabidopsis thaliana\t309275\tEukaryota\tPlantae\tStreptophyta\t",
      "Magnoliopsida\tBrassicales\tBrassicaceae\tNA\tArabidopsis\tArabidopsis thaliana\tNA"
    ),
    ott
  )
  writeLines("filename\torganism\nfile1\tArabidopsis thaliana", metadata)

  expect_error(
    prepare_taxa(
      input = input,
      org_tax_ott = ott,
      taxon = NULL,
      metadata = metadata,
      name_filename = "filename",
      colname = "organism",
      output = temp_test_path("out-no-sample.tsv"),
      extension = FALSE
    ),
    "does not contain the 'sample' column",
    class = "tima_validation_error"
  )
})

test_that("prepare_taxa errors when metadata filename key is missing", {
  input <- temp_test_path("features-no-filename-key.tsv")
  ott <- temp_test_path("ott-no-filename-key.tsv")
  metadata <- temp_test_path("metadata-no-filename-key.tsv")

  writeLines("feature_id\trt\tmz\tsample\nFT001\t1.5\t200\tfile1", input)
  writeLines(
    paste0(
      "organism_name\torganism_taxonomy_ottid\torganism_taxonomy_01domain\t",
      "organism_taxonomy_02kingdom\torganism_taxonomy_03phylum\t",
      "organism_taxonomy_04class\torganism_taxonomy_05order\t",
      "organism_taxonomy_06family\torganism_taxonomy_07tribe\t",
      "organism_taxonomy_08genus\torganism_taxonomy_09species\t",
      "organism_taxonomy_10varietas\n",
      "Arabidopsis thaliana\t309275\tEukaryota\tPlantae\tStreptophyta\t",
      "Magnoliopsida\tBrassicales\tBrassicaceae\tNA\tArabidopsis\tArabidopsis thaliana\tNA"
    ),
    ott
  )
  writeLines("wrong_key\torganism\nfile1\tArabidopsis thaliana", metadata)

  expect_error(
    prepare_taxa(
      input = input,
      org_tax_ott = ott,
      taxon = NULL,
      metadata = metadata,
      name_filename = "filename",
      colname = "organism",
      output = temp_test_path("out-no-filename-key.tsv"),
      extension = TRUE
    ),
    "does not contain the filename column",
    class = "tima_validation_error"
  )
})

test_that("prepare_taxa strips known MS file extensions when extension is FALSE", {
  input <- temp_test_path("features-extension-strip.tsv")
  ott <- temp_test_path("ott-extension-strip.tsv")
  metadata <- temp_test_path("metadata-extension-strip.tsv")
  output <- temp_test_path("output-extension-strip.tsv")

  writeLines(
    paste(
      "feature_id\trt\tmz\tsample",
      "FT001\t1.5\t200\tfileA",
      "FT002\t2.0\t300\tfileB",
      sep = "\n"
    ),
    input
  )

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

  writeLines(
    paste(
      "filename\torganism",
      "fileA.mzML\tOrg1",
      "fileB.mzxML\tOrg2",
      sep = "\n"
    ),
    metadata
  )

  expect_no_error(
    prepare_taxa(
      input = input,
      org_tax_ott = ott,
      taxon = NULL,
      metadata = metadata,
      name_filename = "filename",
      colname = "organism",
      output = output,
      extension = FALSE
    )
  )

  df <- tidytable::fread(output)
  expect_equal(nrow(df), 2L)
  expect_true("Org1" %in% df$sample_organism_name)
  expect_true("Org2" %in% df$sample_organism_name)
})
