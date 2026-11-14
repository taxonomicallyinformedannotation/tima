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
  expect_true(all(
    c(
      "sample_organism_01_domain",
      "sample_organism_02_kingdom",
      "sample_organism_03_phylum",
      "sample_organism_04_class",
      "sample_organism_05_order",
      "sample_organism_05_1_infraorder",
      "sample_organism_06_family",
      "sample_organism_06_1_subfamily",
      "sample_organism_07_tribe",
      "sample_organism_07_1_subtribe",
      "sample_organism_08_genus",
      "sample_organism_08_1_subgenus",
      "sample_organism_09_species",
      "sample_organism_09_1_subspecies",
      "sample_organism_10_varietas"
    ) %in%
      names(df)
  ))
  expect_identical(unique(df$sample_organism_05_1_infraorder), "ND")
  expect_identical(unique(df$sample_organism_06_family), "Hominidae")
  expect_equal(nrow(df), 2) # Both features assigned to same organism
})

test_that("prepare_taxa preserves all 5 sub-ranks and weights cleanly in weight_bio", {
  input <- temp_test_path("features.tsv")
  ott <- temp_test_path("ott_subranks.tsv")
  output <- temp_test_path("output_subranks.tsv")

  writeLines("feature_id\trt\tmz\nFT001\t1.5\t200", input)
  writeLines(
    paste0(
      "organism_name\torganism_taxonomy_ottid\t",
      "organism_taxonomy_01domain\torganism_taxonomy_02kingdom\t",
      "organism_taxonomy_03phylum\torganism_taxonomy_04class\t",
      "organism_taxonomy_05order\torganism_taxonomy_05_1infraorder\t",
      "organism_taxonomy_06family\torganism_taxonomy_06_1subfamily\t",
      "organism_taxonomy_07tribe\torganism_taxonomy_07_1subtribe\t",
      "organism_taxonomy_08genus\torganism_taxonomy_08_1subgenus\t",
      "organism_taxonomy_09species\torganism_taxonomy_09_1subspecies\t",
      "organism_taxonomy_10varietas\n",
      "Homo sapiens neanderthalensis\t990001\tEukaryota\tMetazoa\tChordata\t",
      "Mammalia\tPrimates\tSimiiformes\tHominidae\tHomininae\tHominini\t",
      "Hominina\tHomo\tHomo (subgenus)\tHomo sapiens\t",
      "Homo sapiens neanderthalensis\tNA"
    ),
    ott
  )

  prepare_taxa(
    input = input,
    org_tax_ott = ott,
    taxon = "Homo sapiens neanderthalensis",
    output = output,
    extension = FALSE,
    metadata = NULL
  )
  taxa_tbl <- tidytable::fread(output)
  expect_identical(taxa_tbl$sample_organism_05_1_infraorder, "Simiiformes")
  expect_identical(taxa_tbl$sample_organism_06_1_subfamily, "Homininae")
  expect_identical(taxa_tbl$sample_organism_07_1_subtribe, "Hominina")
  expect_identical(taxa_tbl$sample_organism_08_1_subgenus, "Homo (subgenus)")
  expect_identical(
    taxa_tbl$sample_organism_09_1_subspecies,
    "Homo sapiens neanderthalensis"
  )

  annot_tbl <- tidytable::tidytable(
    feature_id = "FT001",
    candidate_structure_inchikey_connectivity_layer = "IK1",
    candidate_score_similarity = 0.8,
    candidate_score_sirius_csi = NA_real_
  ) |>
    tidytable::left_join(taxa_tbl, by = "feature_id")

  sop_tbl <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = "IK1",
    organism_name = "Other primate",
    organism_taxonomy_ottid = "990099",
    organism_taxonomy_01domain = "Eukaryota",
    organism_taxonomy_02kingdom = "Metazoa",
    organism_taxonomy_03phylum = "Chordata",
    organism_taxonomy_04class = "Mammalia",
    organism_taxonomy_05order = "Primates",
    organism_taxonomy_05_1infraorder = "Simiiformes",
    organism_taxonomy_06family = "OtherFamily",
    organism_taxonomy_06_1subfamily = "OtherSubfamily",
    organism_taxonomy_07tribe = "OtherTribe",
    organism_taxonomy_07_1subtribe = "OtherSubtribe",
    organism_taxonomy_08genus = "OtherGenus",
    organism_taxonomy_08_1subgenus = "OtherSubgenus",
    organism_taxonomy_09species = "OtherSpecies",
    organism_taxonomy_09_1subspecies = "OtherSubspecies",
    organism_taxonomy_10varietas = "OtherVariety"
  )

  weighted <- weight_bio(
    annotation_table_taxed = annot_tbl,
    structure_organism_pairs_table = sop_tbl,
    weight_spectral = 0.5,
    weight_biological = 0.5,
    score_biological_domain = 0.1,
    score_biological_kingdom = 0.2,
    score_biological_phylum = 0.3,
    score_biological_class = 0.4,
    score_biological_order = 0.5,
    score_biological_infraorder = 0.55,
    score_biological_family = 0.6,
    score_biological_subfamily = 0.65,
    score_biological_tribe = 0.7,
    score_biological_subtribe = 0.75,
    score_biological_genus = 0.8,
    score_biological_subgenus = 0.85,
    score_biological_species = 0.9,
    score_biological_subspecies = 0.95,
    score_biological_variety = 1.0,
    score_biological_biota = 1.0
  )
  expect_equal(weighted$score_biological, 0.55)
})

test_that("prepare_taxa works with multiple taxon assignment", {
  skip_if_no_network("https://api.opentreeoflife.org")
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

test_that("prepare_taxa high-volume combination collapse (>256 rows) matches small-table collapse", {
  input <- temp_test_path("features-high-vol.tsv")
  ott <- temp_test_path("ott-high-vol.tsv")
  metadata <- temp_test_path("metadata-high-vol.tsv")
  output <- temp_test_path("output-high-vol.tsv")

  f_ids <- sprintf("FT%04d", 1:150)
  feat_df <- tidytable::tidytable(
    feature_id = rep(f_ids, each = 2L),
    rt = "2.5",
    mz = "300.1",
    sample = rep(c("s1", "s2", "s3"), length.out = 300L)
  )
  tidytable::fwrite(feat_df, input, sep = "\t")

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
      "s1\tOrg1 | Org2",
      "s2\tOrg1",
      "s3\tOrg2",
      sep = "\n"
    ),
    metadata
  )

  prepare_taxa(
    input = input,
    org_tax_ott = ott,
    taxon = NULL,
    metadata = metadata,
    name_filename = "filename",
    colname = "organism",
    output = output,
    extension = TRUE
  )

  df <- tidytable::fread(output)
  expect_equal(nrow(df), 150L)
  expect_identical(df$feature_id, f_ids)
  expect_true(all(grepl("Org1", df$sample_organism_name)))
  expect_true(all(grepl("Org2", df$sample_organism_name)))

  # Also test high-volume single-organism per feature fast path (>256 rows)
  input_single <- temp_test_path("features-high-vol-single.tsv")
  output_single <- temp_test_path("output-high-vol-single.tsv")
  f_ids_s <- sprintf("FTS%04d", 1:300)
  feat_single <- tidytable::tidytable(
    feature_id = f_ids_s,
    rt = "2.5",
    mz = "300.1",
    sample = rep(c("s2", "s3"), length.out = 300L)
  )
  tidytable::fwrite(feat_single, input_single, sep = "\t")

  prepare_taxa(
    input = input_single,
    org_tax_ott = ott,
    taxon = NULL,
    metadata = metadata,
    name_filename = "filename",
    colname = "organism",
    output = output_single,
    extension = TRUE
  )
  df_s <- tidytable::fread(output_single)
  expect_equal(nrow(df_s), 300L)
  expect_identical(df_s$feature_id, f_ids_s)
  expect_identical(df_s$sample_organism_name[1:2], c("Org1", "Org2"))
})

test_that("prepare_taxa covers intensity filtering, missing OTT lookups, missing columns, and >256 mixed batching", {
  ott <- temp_test_path("ott-cov.tsv")
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

  # 1. Missing feature_id column in features file with single taxon -> hits wanted_feat_cols == 0,
  # else (!is.null(taxon)) branch, and missing_select_cols abort
  input_nofid <- temp_test_path("features-nofid.tsv")
  output_nofid <- temp_test_path("output-nofid.tsv")
  writeLines("rt\tmz\n1.5\t200.0", input_nofid)
  expect_error(
    prepare_taxa(
      input = input_nofid,
      org_tax_ott = ott,
      taxon = "Org1",
      output = output_nofid,
      extension = TRUE,
      metadata = NULL
    ),
    "Missing columns"
  )

  # 2. Metadata column present but all organism values NA/empty -> exports ND taxonomy
  input_nd <- temp_test_path("features-nd.tsv")
  meta_nd <- temp_test_path("meta-nd.tsv")
  output_nd <- temp_test_path("output-nd.tsv")
  writeLines("feature_id\tsample\nFT1\ts1", input_nd)
  writeLines("filename\torganism\ns1\t", meta_nd)
  prepare_taxa(
    input = input_nd,
    org_tax_ott = ott,
    taxon = NULL,
    metadata = meta_nd,
    name_filename = "filename",
    colname = "organism",
    output = output_nd,
    extension = TRUE
  )
  df_nd <- tidytable::fread(output_nd)
  expect_identical(df_nd$sample_organism_name, "ND")

  # 3. Missing organism in OTT triggers get_organism_taxonomy_ott
  local_mocked_bindings(
    get_organism_taxonomy_ott = function(df) {
      tidytable::tidytable(
        organism_name = df$organism,
        organism_taxonomy_ottid = "9999",
        organism_taxonomy_01domain = "Eukaryota",
        organism_taxonomy_02kingdom = "Plantae",
        organism_taxonomy_03phylum = NA_character_,
        organism_taxonomy_04class = NA_character_,
        organism_taxonomy_05order = NA_character_,
        organism_taxonomy_06family = NA_character_,
        organism_taxonomy_07tribe = NA_character_,
        organism_taxonomy_08genus = "Genus3",
        organism_taxonomy_09species = df$organism,
        organism_taxonomy_10varietas = NA_character_
      )
    }
  )
  meta_new <- temp_test_path("meta-new.tsv")
  output_new <- temp_test_path("output-new.tsv")
  writeLines("filename\torganism\ns1\tOrg1|Org3", meta_new)
  prepare_taxa(
    input = input_nd,
    org_tax_ott = ott,
    taxon = NULL,
    metadata = meta_new,
    name_filename = "filename",
    colname = "organism",
    output = output_new,
    extension = TRUE
  )
  df_new <- tidytable::fread(output_new)
  expect_true(grepl("Org3", df_new$sample_organism_name))

  # 4. Intensity-weighted taxon attribution (both unique-filename and pipe-expanded metadata)
  input_int <- temp_test_path("features-int.tsv")
  meta_int1 <- temp_test_path("meta-int1.tsv")
  meta_int2 <- temp_test_path("meta-int2.tsv")
  output_int1 <- temp_test_path("output-int1.tsv")
  output_int2 <- temp_test_path("output-int2.tsv")
  writeLines(
    "feature_id\tsample\tintensity\nFT1\ts1\t1000\nFT1\ts2\t1\nFT2\ts1\t500",
    input_int
  )
  writeLines("filename\torganism\ns1\tOrg1\ns2\tOrg2", meta_int1)
  prepare_taxa(
    input = input_int,
    org_tax_ott = ott,
    taxon = NULL,
    metadata = meta_int1,
    name_filename = "filename",
    colname = "organism",
    output = output_int1,
    extension = TRUE,
    min_rel_intensity = 0.05
  )
  df_int1 <- tidytable::fread(output_int1)
  expect_identical(
    df_int1$sample_organism_name[df_int1$feature_id == "FT1"],
    "Org1"
  )

  writeLines("filename\torganism\ns1\tOrg1|Org2\ns2\tOrg2", meta_int2)
  prepare_taxa(
    input = input_int,
    org_tax_ott = ott,
    taxon = NULL,
    metadata = meta_int2,
    name_filename = "filename",
    colname = "organism",
    output = output_int2,
    extension = TRUE,
    min_rel_intensity = 0.05
  )
  expect_true(file.exists(output_int2))

  # 5. 0-row features with metadata -> exports ND taxonomy
  input_empty_f <- temp_test_path("features-empty.tsv")
  output_empty_f <- temp_test_path("output-empty.tsv")
  writeLines("feature_id\tsample\tintensity", input_empty_f)
  prepare_taxa(
    input = input_empty_f,
    org_tax_ott = ott,
    taxon = NULL,
    metadata = meta_int1,
    name_filename = "filename",
    colname = "organism",
    output = output_empty_f,
    extension = TRUE
  )
  expect_equal(nrow(tidytable::fread(output_empty_f)), 0L)

  # 6. >256 rows with whitespace in organism, NA organism (unmatched sample), and mixed 1- and 2-org features in interleaved order
  input_mix <- temp_test_path("features-mix.tsv")
  meta_mix <- temp_test_path("meta-mix.tsv")
  output_mix <- temp_test_path("output-mix.tsv")
  f_interleaved <- c(
    sprintf("FTM%04d", 1:200),
    sprintf("FTM%04d", 1:100)
  )
  s_interleaved <- c(
    rep(c("s1", "s_unmatched"), length.out = 200L),
    rep("s2", 100L)
  )
  tidytable::fwrite(
    tidytable::tidytable(feature_id = f_interleaved, sample = s_interleaved),
    input_mix,
    sep = "\t"
  )
  writeLines("filename\torganism\ns1\t Org1 \ns2\tOrg2", meta_mix)
  prepare_taxa(
    input = input_mix,
    org_tax_ott = ott,
    taxon = NULL,
    metadata = meta_mix,
    name_filename = "filename",
    colname = "organism",
    output = output_mix,
    extension = TRUE
  )
  df_mix <- tidytable::fread(output_mix)
  expect_equal(nrow(df_mix), 200L)
})
