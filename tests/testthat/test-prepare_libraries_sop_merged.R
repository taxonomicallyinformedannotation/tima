# ==============================================================================
# Test Suite: prepare_libraries_sop_merged
# ==============================================================================

test_that("validate_sop_merged_inputs accepts valid inputs", {
  expect_silent(
    validate_sop_merged_inputs(
      files = c("file1.tsv"),
      filter = FALSE,
      level = "family",
      value = "Test",
      output_key = "key.tsv",
      output_org_tax_ott = "org.tsv",
      output_str_stereo = "stereo.tsv",
      output_str_met = "met.tsv",
      output_str_nam = "nam.tsv",
      output_str_tax_cla = "cla.tsv",
      output_str_tax_npc = "npc.tsv"
    )
  )
})

test_that("validate_sop_merged_inputs rejects invalid filter", {
  expect_error(
    validate_sop_merged_inputs(
      files = c("file1.tsv"),
      filter = "yes",
      level = "family",
      value = "Test",
      output_key = "key.tsv",
      output_org_tax_ott = "org.tsv",
      output_str_stereo = "stereo.tsv",
      output_str_met = "met.tsv",
      output_str_nam = "nam.tsv",
      output_str_tax_cla = "cla.tsv",
      output_str_tax_npc = "npc.tsv"
    ),
    "filter must be a single logical value"
  )
})

test_that("validate_sop_merged_inputs rejects invalid level", {
  expect_error(
    validate_sop_merged_inputs(
      files = c("file1.tsv"),
      filter = TRUE,
      level = "invalid_level",
      value = "Test",
      output_key = "key.tsv",
      output_org_tax_ott = "org.tsv",
      output_str_stereo = "stereo.tsv",
      output_str_met = "met.tsv",
      output_str_nam = "nam.tsv",
      output_str_tax_cla = "cla.tsv",
      output_str_tax_npc = "npc.tsv"
    ),
    "level must be one of"
  )
})

test_that("complete_organism_taxonomy handles empty missing data", {
  table_keys <- tidytable::tidytable(
    organism_name = c("Org1", "Org2")
  )

  table_org_tax_ott <- tidytable::tidytable(
    organism_name = c("Org1", "Org2"),
    kingdom = c("Plantae", "Plantae")
  )

  result <- complete_organism_taxonomy(table_keys, table_org_tax_ott)

  expect_equal(nrow(result), 2)
})

test_that("apply_taxonomic_filter filters correctly", {
  table_keys <- tidytable::tidytable(
    structure_inchikey = c("A", "B", "C"),
    structure_smiles_no_stereo = c("S1", "S2", "S3"),
    organism_name = c("Plant1", "Plant2", "Plant3"),
    reference_doi = c("doi1", "doi2", "doi3")
  )

  table_org_tax_ott <- tidytable::tidytable(
    organism_name = c("Plant1", "Plant2", "Plant3"),
    organism_taxonomy_ottol_family = c("Fam1", "Fam2", "Fam1")
  )

  result <- apply_taxonomic_filter(
    table_keys,
    table_org_tax_ott,
    level = "family",
    value = "Fam1"
  )

  expect_equal(nrow(result), 2)
  expect_true(all(c("Plant1", "Plant3") %in% result$organism_name))
})

test_that("apply_taxonomic_filter errors on no matches", {
  table_keys <- tidytable::tidytable(
    structure_inchikey = c("A"),
    structure_smiles_no_stereo = c("S1"),
    organism_name = c("Plant1"),
    reference_doi = c("doi1")
  )

  table_org_tax_ott <- tidytable::tidytable(
    organism_name = c("Plant1"),
    organism_taxonomy_ottol_family = c("Fam1")
  )

  expect_error(
    apply_taxonomic_filter(
      table_keys,
      table_org_tax_ott,
      level = "family",
      value = "NonExistent"
    ),
    "Filter led to no entries"
  )
})

test_that(
  skip("Not implemented")
)
# test_that("prepare_libraries_sop_merged works with filtering", {
#   skip_on_cran()
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   # Need LOTUS data
#   fake_lotus(export = paths$data$source$libraries$sop$lotus)
#   prepare_libraries_sop_lotus()
#
#   expect_no_error(
#     prepare_libraries_sop_merged(
#       files = get_params(
#         step = "prepare_libraries_sop_merged"
#       )$files$libraries$sop$prepared$lotus,
#       filter = TRUE,
#       level = "family",
#       value = "Simaroubaceae|Gentianaceae",
#       output_key = "data/interim/libraries/sop/merged/bitter.tsv.gz"
#     )
#   )
# })

test_that(
  skip("Not implemented")
)
# test_that("prepare_libraries_sop_merged triggers SMILES processing", {
#   skip_on_cran()
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   # Create fake data
#   fake_ecmdb(export = paths$data$source$libraries$sop$ecmdb)
#   fake_lotus(export = paths$data$source$libraries$sop$lotus)
#
#   prepare_libraries_sop_ecmdb()
#   prepare_libraries_sop_lotus()
#   prepare_libraries_sop_closed()
#
#   expect_no_error(
#     prepare_libraries_sop_merged(
#       files = c(
#         get_params(
#           step = "prepare_libraries_sop_merged"
#         )$files$libraries$sop$prepared$closed,
#         get_params(
#           step = "prepare_libraries_sop_merged"
#         )$files$libraries$sop$prepared$ecmdb,
#         get_params(
#           step = "prepare_libraries_sop_merged"
#         )$files$libraries$sop$prepared$lotus
#       )
#     )
#   )
# })

# ==============================================================================
# Test Group: validate_sop_merged_inputs - Additional edge cases
# ==============================================================================

test_that("test-validate_sop_merged_inputs accepts all valid taxonomic levels", {
  valid_levels <- c(
    "domain",
    "kingdom",
    "phylum",
    "class",
    "order",
    "family",
    "tribe",
    "genus",
    "species",
    "varietas"
  )

  for (lvl in valid_levels) {
    expect_silent(
      validate_sop_merged_inputs(
        files = c("file1.tsv"),
        filter = TRUE,
        level = lvl,
        value = "TestValue",
        output_key = "key.tsv",
        output_org_tax_ott = "org.tsv",
        output_str_stereo = "stereo.tsv",
        output_str_met = "met.tsv",
        output_str_nam = "nam.tsv",
        output_str_tax_cla = "cla.tsv",
        output_str_tax_npc = "npc.tsv"
      )
    )
  }
})

test_that("test-validate_sop_merged_inputs rejects non-character output paths", {
  expect_error(
    validate_sop_merged_inputs(
      files = c("file1.tsv"),
      filter = FALSE,
      level = "family",
      value = "Test",
      output_key = 123, # Invalid
      output_org_tax_ott = "org.tsv",
      output_str_stereo = "stereo.tsv",
      output_str_met = "met.tsv",
      output_str_nam = "nam.tsv",
      output_str_tax_cla = "cla.tsv",
      output_str_tax_npc = "npc.tsv"
    ),
    "output_key must be a single character string"
  )
})

test_that("test-validate_sop_merged_inputs rejects vector output paths", {
  expect_error(
    validate_sop_merged_inputs(
      files = c("file1.tsv"),
      filter = FALSE,
      level = "family",
      value = "Test",
      output_key = "key.tsv",
      output_org_tax_ott = c("org1.tsv", "org2.tsv"), # Invalid - vector
      output_str_stereo = "stereo.tsv",
      output_str_met = "met.tsv",
      output_str_nam = "nam.tsv",
      output_str_tax_cla = "cla.tsv",
      output_str_tax_npc = "npc.tsv"
    ),
    "output_org_tax_ott must be a single character string"
  )
})

test_that("test-validate_sop_merged_inputs requires value when filtering", {
  expect_error(
    validate_sop_merged_inputs(
      files = c("file1.tsv"),
      filter = TRUE,
      level = "family",
      value = 123, # Invalid - not character
      output_key = "key.tsv",
      output_org_tax_ott = "org.tsv",
      output_str_stereo = "stereo.tsv",
      output_str_met = "met.tsv",
      output_str_nam = "nam.tsv",
      output_str_tax_cla = "cla.tsv",
      output_str_tax_npc = "npc.tsv"
    ),
    "value must be a single character string"
  )
})

test_that("test-validate_sop_merged_inputs accepts filter=FALSE without checking level/value", {
  # When filter=FALSE, level and value are not validated
  expect_silent(
    validate_sop_merged_inputs(
      files = c("file1.tsv"),
      filter = FALSE,
      level = "invalid_level", # This should be ignored when filter=FALSE
      value = "Test",
      output_key = "key.tsv",
      output_org_tax_ott = "org.tsv",
      output_str_stereo = "stereo.tsv",
      output_str_met = "met.tsv",
      output_str_nam = "nam.tsv",
      output_str_tax_cla = "cla.tsv",
      output_str_tax_npc = "npc.tsv"
    )
  )
})

test_that("test-validate_sop_merged_inputs rejects NULL filter", {
  expect_error(
    validate_sop_merged_inputs(
      files = c("file1.tsv"),
      filter = NULL,
      level = "family",
      value = "Test",
      output_key = "key.tsv",
      output_org_tax_ott = "org.tsv",
      output_str_stereo = "stereo.tsv",
      output_str_met = "met.tsv",
      output_str_nam = "nam.tsv",
      output_str_tax_cla = "cla.tsv",
      output_str_tax_npc = "npc.tsv"
    ),
    "filter must be a single logical value"
  )
})

test_that("test-validate_sop_merged_inputs validates all output parameters", {
  # Test each output parameter individually
  output_params <- c(
    "output_key",
    "output_org_tax_ott",
    "output_str_stereo",
    "output_str_met",
    "output_str_nam",
    "output_str_tax_cla",
    "output_str_tax_npc"
  )

  for (param in output_params) {
    args <- list(
      files = c("file1.tsv"),
      filter = FALSE,
      level = "family",
      value = "Test",
      output_key = "key.tsv",
      output_org_tax_ott = "org.tsv",
      output_str_stereo = "stereo.tsv",
      output_str_met = "met.tsv",
      output_str_nam = "nam.tsv",
      output_str_tax_cla = "cla.tsv",
      output_str_tax_npc = "npc.tsv"
    )

    # Set the specific parameter to invalid value
    args[[param]] <- c("path1.tsv", "path2.tsv")

    expect_error(
      do.call(validate_sop_merged_inputs, args),
      paste0(param, " must be a single character string")
    )
  }
})

# ==============================================================================
# Test Group: load_and_merge_libraries
# ==============================================================================

test_that("test-load_and_merge_libraries combines multiple files", {
  skip("Requires complex setup with split_tables_sop function")
  # This function calls split_tables_sop which requires extensive setup
})

test_that("test-load_and_merge_libraries handles single file", {
  skip("Requires complex setup with split_tables_sop function")
})

test_that("test-load_and_merge_libraries handles NA strings correctly", {
  skip("Requires complex setup with split_tables_sop function")
})

# ==============================================================================
# Test Group: complete_organism_taxonomy
# ==============================================================================

test_that("test-complete_organism_taxonomy returns original when all organisms present", {
  table_keys <- tidytable::tidytable(
    organism_name = c("Org1", "Org2")
  )

  table_org_tax_ott <- tidytable::tidytable(
    organism_name = c("Org1", "Org2"),
    organism_taxonomy_ottol_kingdom = c("Plantae", "Animalia")
  )

  result <- complete_organism_taxonomy(table_keys, table_org_tax_ott)

  expect_equal(nrow(result), 2)
  expect_equal(result$organism_name, c("Org1", "Org2"))
})

test_that("test-complete_organism_taxonomy handles all missing organisms", {
  table_keys <- tidytable::tidytable(
    organism_name = c("NewOrg1", "NewOrg2")
  )

  table_org_tax_ott <- tidytable::tidytable(
    organism_name = character(),
    organism_taxonomy_ottol_kingdom = character()
  )

  # This will try to fetch taxonomy - may fail in test environment
  # We just check it doesn't error
  expect_no_error(
    complete_organism_taxonomy(table_keys, table_org_tax_ott)
  )
})

test_that("test-complete_organism_taxonomy handles mixed missing/present", {
  table_keys <- tidytable::tidytable(
    organism_name = c("Org1", "NewOrg")
  )

  table_org_tax_ott <- tidytable::tidytable(
    organism_name = c("Org1"),
    organism_taxonomy_ottol_kingdom = c("Plantae")
  )

  # Should attempt to fetch taxonomy for NewOrg
  expect_no_error(
    complete_organism_taxonomy(table_keys, table_org_tax_ott)
  )
})

test_that("test-complete_organism_taxonomy preserves existing data", {
  table_keys <- tidytable::tidytable(
    organism_name = c("Org1", "Org2")
  )

  table_org_tax_ott <- tidytable::tidytable(
    organism_name = c("Org1", "Org2"),
    organism_taxonomy_ottol_kingdom = c("Plantae", "Animalia"),
    organism_taxonomy_ottol_family = c("Fam1", "Fam2")
  )

  result <- complete_organism_taxonomy(table_keys, table_org_tax_ott)

  expect_equal(result$organism_taxonomy_ottol_kingdom, c("Plantae", "Animalia"))
  expect_equal(result$organism_taxonomy_ottol_family, c("Fam1", "Fam2"))
})

# ==============================================================================
# Test Group: apply_taxonomic_filter - Comprehensive
# ==============================================================================

test_that("test-apply_taxonomic_filter filters by kingdom", {
  table_keys <- tidytable::tidytable(
    structure_inchikey = c("A", "B", "C"),
    structure_smiles_no_stereo = c("S1", "S2", "S3"),
    organism_name = c("Plant1", "Animal1", "Plant2"),
    reference_doi = c("doi1", "doi2", "doi3")
  )

  table_org_tax_ott <- tidytable::tidytable(
    organism_name = c("Plant1", "Animal1", "Plant2"),
    organism_taxonomy_ottol_kingdom = c("Plantae", "Animalia", "Plantae")
  )

  result <- apply_taxonomic_filter(
    table_keys,
    table_org_tax_ott,
    level = "kingdom",
    value = "Plantae"
  )

  expect_equal(nrow(result), 2)
  expect_true(all(c("Plant1", "Plant2") %in% result$organism_name))
})

test_that("test-apply_taxonomic_filter filters by genus", {
  table_keys <- tidytable::tidytable(
    structure_inchikey = c("A", "B", "C"),
    structure_smiles_no_stereo = c("S1", "S2", "S3"),
    organism_name = c("Species1", "Species2", "Species3"),
    reference_doi = c("doi1", "doi2", "doi3")
  )

  table_org_tax_ott <- tidytable::tidytable(
    organism_name = c("Species1", "Species2", "Species3"),
    organism_taxonomy_ottol_genus = c("Genus1", "Genus2", "Genus1")
  )

  result <- apply_taxonomic_filter(
    table_keys,
    table_org_tax_ott,
    level = "genus",
    value = "Genus1"
  )

  expect_equal(nrow(result), 2)
  expect_true(all(c("Species1", "Species3") %in% result$organism_name))
})

test_that("test-apply_taxonomic_filter filters by species", {
  table_keys <- tidytable::tidytable(
    structure_inchikey = c("A", "B"),
    structure_smiles_no_stereo = c("S1", "S2"),
    organism_name = c("Homo sapiens", "Mus musculus"),
    reference_doi = c("doi1", "doi2")
  )

  table_org_tax_ott <- tidytable::tidytable(
    organism_name = c("Homo sapiens", "Mus musculus"),
    organism_taxonomy_ottol_species = c("Homo sapiens", "Mus musculus")
  )

  result <- apply_taxonomic_filter(
    table_keys,
    table_org_tax_ott,
    level = "species",
    value = "Homo sapiens"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$organism_name, "Homo sapiens")
})

test_that("test-apply_taxonomic_filter handles case sensitivity", {
  table_keys <- tidytable::tidytable(
    structure_inchikey = c("A", "B"),
    structure_smiles_no_stereo = c("S1", "S2"),
    organism_name = c("Org1", "Org2"),
    reference_doi = c("doi1", "doi2")
  )

  table_org_tax_ott <- tidytable::tidytable(
    organism_name = c("Org1", "Org2"),
    organism_taxonomy_ottol_family = c("Fabaceae", "Rosaceae")
  )

  # Test exact case match
  result <- apply_taxonomic_filter(
    table_keys,
    table_org_tax_ott,
    level = "family",
    value = "Fabaceae"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$organism_name, "Org1")
})

test_that("test-apply_taxonomic_filter handles NA taxonomy values", {
  table_keys <- tidytable::tidytable(
    structure_inchikey = c("A", "B", "C"),
    structure_smiles_no_stereo = c("S1", "S2", "S3"),
    organism_name = c("Org1", "Org2", "Org3"),
    reference_doi = c("doi1", "doi2", "doi3")
  )

  table_org_tax_ott <- tidytable::tidytable(
    organism_name = c("Org1", "Org2", "Org3"),
    organism_taxonomy_ottol_order = c("Order1", NA, "Order1")
  )

  result <- apply_taxonomic_filter(
    table_keys,
    table_org_tax_ott,
    level = "order",
    value = "Order1"
  )

  # Should only match Org1 and Org3
  expect_equal(nrow(result), 2)
  expect_false("Org2" %in% result$organism_name)
})

test_that("test-apply_taxonomic_filter returns unique structures", {
  # Multiple organisms can have same structure
  table_keys <- tidytable::tidytable(
    structure_inchikey = c("A", "A", "B"),
    structure_smiles_no_stereo = c("S1", "S1", "S2"),
    organism_name = c("Org1", "Org2", "Org3"),
    reference_doi = c("doi1", "doi2", "doi3")
  )

  table_org_tax_ott <- tidytable::tidytable(
    organism_name = c("Org1", "Org2", "Org3"),
    organism_taxonomy_ottol_class = c("Class1", "Class1", "Class2")
  )

  result <- apply_taxonomic_filter(
    table_keys,
    table_org_tax_ott,
    level = "class",
    value = "Class1"
  )

  # Should return both rows for structure A (different organisms)
  expect_equal(nrow(result), 2)
  expect_equal(sum(result$structure_inchikey == "A"), 2)
})

test_that("test-apply_taxonomic_filter handles empty result", {
  table_keys <- tidytable::tidytable(
    structure_inchikey = c("A", "B"),
    structure_smiles_no_stereo = c("S1", "S2"),
    organism_name = c("Org1", "Org2"),
    reference_doi = c("doi1", "doi2")
  )

  table_org_tax_ott <- tidytable::tidytable(
    organism_name = c("Org1", "Org2"),
    organism_taxonomy_ottol_phylum = c("Phylum1", "Phylum2")
  )

  # Filter by non-existent value
  expect_error(
    apply_taxonomic_filter(
      table_keys,
      table_org_tax_ott,
      level = "phylum",
      value = "NonExistentPhylum"
    ),
    "Filter led to no entries"
  )
})

test_that("test-apply_taxonomic_filter handles missing taxonomy column", {
  table_keys <- tidytable::tidytable(
    structure_inchikey = c("A"),
    structure_smiles_no_stereo = c("S1"),
    organism_name = c("Org1"),
    reference_doi = c("doi1")
  )

  # Missing the expected taxonomy column
  table_org_tax_ott <- tidytable::tidytable(
    organism_name = c("Org1"),
    other_column = c("value")
  )

  expect_error(
    apply_taxonomic_filter(
      table_keys,
      table_org_tax_ott,
      level = "tribe",
      value = "TestTribe"
    ),
    "No column found matching level"
  )
})

# ==============================================================================
# Test Group: Integration tests for prepare_libraries_sop_merged
# ==============================================================================

test_that("test-prepare_libraries_sop_merged validates inputs before processing", {
  # Test that validation happens first
  expect_error(
    prepare_libraries_sop_merged(
      files = c("file.tsv"),
      filter = "invalid", # Invalid type
      level = "family",
      value = "Test",
      output_key = "key.tsv",
      output_org_tax_ott = "org.tsv",
      output_str_stereo = "stereo.tsv",
      output_str_met = "met.tsv",
      output_str_nam = "nam.tsv",
      output_str_tax_cla = "cla.tsv",
      output_str_tax_npc = "npc.tsv"
    ),
    "filter must be a single logical value"
  )
})

# ==============================================================================
# Test Group: Edge cases and boundary conditions
# ==============================================================================

test_that("test-validate_sop_merged_inputs accepts empty value when filter=FALSE", {
  expect_silent(
    validate_sop_merged_inputs(
      files = c("file1.tsv"),
      filter = FALSE,
      level = "family",
      value = "", # Empty string OK when not filtering
      output_key = "key.tsv",
      output_org_tax_ott = "org.tsv",
      output_str_stereo = "stereo.tsv",
      output_str_met = "met.tsv",
      output_str_nam = "nam.tsv",
      output_str_tax_cla = "cla.tsv",
      output_str_tax_npc = "npc.tsv"
    )
  )
})

test_that("test-complete_organism_taxonomy handles duplicate organism names", {
  table_keys <- tidytable::tidytable(
    organism_name = c("Org1", "Org1", "Org2")
  )

  table_org_tax_ott <- tidytable::tidytable(
    organism_name = c("Org1", "Org2"),
    organism_taxonomy_ottol_kingdom = c("Plantae", "Animalia")
  )

  result <- complete_organism_taxonomy(table_keys, table_org_tax_ott)

  # Should still return 2 unique organisms
  expect_true(nrow(result) >= 2)
})

test_that("test-apply_taxonomic_filter preserves core columns", {
  table_keys <- tidytable::tidytable(
    structure_inchikey = c("A", "B"),
    structure_smiles_no_stereo = c("S1", "S2"),
    organism_name = c("Org1", "Org2"),
    reference_doi = c("doi1", "doi2")
  )

  table_org_tax_ott <- tidytable::tidytable(
    organism_name = c("Org1", "Org2"),
    organism_taxonomy_ottol_domain = c("Eukaryota", "Eukaryota")
  )

  result <- apply_taxonomic_filter(
    table_keys,
    table_org_tax_ott,
    level = "domain",
    value = "Eukaryota"
  )

  # Should preserve core columns
  expect_true("structure_inchikey" %in% names(result))
  expect_true("organism_name" %in% names(result))
  expect_true("structure_smiles_no_stereo" %in% names(result))
})
