# Test Suite: select_sop_columns ----

library(testthat)

# Minimal fixture with all required columns
sop_row <- function(n = 1L) {
  tidytable::tidytable(
    structure_name = rep("Gentiopicroside", n),
    structure_inchikey = rep("GQJVWNUATPBVMU-UHFFFAOYSA-N", n),
    structure_smiles = rep("OC[C@@H]1OC(=O)", n),
    structure_inchikey_2D = rep("GQJVWNUATPBVMU", n),
    structure_smiles_2D = rep("OCC1OC(=O)", n),
    structure_molecular_formula = rep("C16H22O9", n),
    structure_exact_mass = rep(358.126, n),
    structure_xlogp = rep(-1.1, n),
    structure_taxonomy_npclassifier_01pathway = rep("Terpenoids", n),
    structure_taxonomy_npclassifier_02superclass = rep("Iridoids", n),
    structure_taxonomy_npclassifier_03class = rep("Secoiridoids", n),
    structure_taxonomy_classyfire_chemontid = rep("CHEMONTID:0000000", n),
    structure_taxonomy_classyfire_01kingdom = rep("Organic compounds", n),
    structure_taxonomy_classyfire_02superclass = rep("Lipids", n),
    structure_taxonomy_classyfire_03class = rep("Fatty acids", n),
    structure_taxonomy_classyfire_04directparent = rep("Carboxylic acids", n),
    organism_name = rep("Gentiana lutea", n),
    organism_taxonomy_ottid = rep("770315", n),
    organism_taxonomy_01domain = rep("Eukaryota", n),
    organism_taxonomy_02kingdom = rep("Plantae", n),
    organism_taxonomy_03phylum = rep("Tracheophyta", n),
    organism_taxonomy_04class = rep("Magnoliopsida", n),
    organism_taxonomy_05order = rep("Gentianales", n),
    organism_taxonomy_06family = rep("Gentianaceae", n),
    organism_taxonomy_07tribe = rep(NA_character_, n),
    organism_taxonomy_08genus = rep("Gentiana", n),
    organism_taxonomy_09species = rep("Gentiana lutea", n),
    organism_taxonomy_10varietas = rep(NA_character_, n),
    reference_doi = rep("10.1234/example", n),
    tag = rep("plant", n),
    extra_col = rep("drop_me", n)
  )
}

test_that("select_sop_columns strips extra columns and renames correctly", {
  out <- select_sop_columns(sop_row())
  expect_false("extra_col" %in% names(out))
  expect_false("structure_inchikey_2D" %in% names(out))
  expect_true("structure_inchikey_connectivity_layer" %in% names(out))
  expect_true("structure_smiles_no_stereo" %in% names(out))
  expect_true("structure_tax_npc_01pat" %in% names(out))
  expect_true("structure_tax_cla_04dirpar" %in% names(out))
  expect_true("tag" %in% names(out))
})

test_that("select_sop_columns has exactly the expected column count", {
  # 8 structure + 3 NPC + 5 ClassyFire + 12 organism + 1 doi + 1 tag = 30
  out <- select_sop_columns(sop_row())
  expect_equal(ncol(out), 30L)
})

test_that("select_sop_columns preserves values through rename", {
  df <- sop_row()
  out <- select_sop_columns(df)
  expect_equal(
    out$structure_inchikey_connectivity_layer,
    df$structure_inchikey_2D
  )
  expect_equal(out$structure_smiles_no_stereo, df$structure_smiles_2D)
  expect_equal(
    out$structure_tax_npc_01pat,
    df$structure_taxonomy_npclassifier_01pathway
  )
  expect_equal(
    out$structure_tax_cla_04dirpar,
    df$structure_taxonomy_classyfire_04directparent
  )
  expect_equal(out$tag, df$tag)
})

test_that("select_sop_columns propagates NA taxonomy without corruption", {
  df <- sop_row(3L)
  df$organism_taxonomy_07tribe[2] <- "Tribe"
  out <- select_sop_columns(df)
  expect_true(is.na(out$organism_taxonomy_07tribe[1]))
  expect_equal(out$organism_taxonomy_07tribe[2], "Tribe")
})

test_that("select_sop_columns rejects non-data-frame input", {
  expect_error(select_sop_columns("not_a_df"), "data.*frame")
  expect_error(select_sop_columns(list(a = 1)), "data.*frame")
})

test_that("select_sop_columns returns zero-row result for empty input", {
  out <- select_sop_columns(sop_row(0L))
  expect_equal(nrow(out), 0L)
})
