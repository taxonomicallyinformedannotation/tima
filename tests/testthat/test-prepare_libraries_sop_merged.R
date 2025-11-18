# ==============================================================================
# Test Suite: prepare_libraries_sop_merged
# ==============================================================================

test_that("validate_sop_merged_inputs accepts valid inputs", {
  expect_silent(
    tima:::validate_sop_merged_inputs(
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
    tima:::validate_sop_merged_inputs(
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
    tima:::validate_sop_merged_inputs(
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

  result <- tima:::complete_organism_taxonomy(table_keys, table_org_tax_ott)

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

  result <- tima:::apply_taxonomic_filter(
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
    tima:::apply_taxonomic_filter(
      table_keys,
      table_org_tax_ott,
      level = "family",
      value = "NonExistent"
    ),
    "Filter led to no entries"
  )
})
