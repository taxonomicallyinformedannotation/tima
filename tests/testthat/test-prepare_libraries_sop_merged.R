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
