# ==============================================================================
# Test Suite: complement_metadata_structures
# ==============================================================================
library(testthat)
library(tima)

test_that("complement_metadata_structures handles empty input", {
  result <- complement_metadata_structures(tidytable::tidytable())
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("complement_metadata_structures enriches metadata", {
  tmp <- withr::local_tempdir(); withr::local_dir(tmp)
  # Minimal df
  df <- tidytable::tidytable(candidate_structure_inchikey_connectivity_layer="INK1", candidate_structure_smiles_no_stereo="C")
  # Create minimal files with required columns
  stereo <- tidytable::tidytable(structure_inchikey_connectivity_layer="INK1", structure_smiles_no_stereo="C")
  tidytable::fwrite(stereo, file="stereo.tsv")
  met <- tidytable::tidytable(structure_inchikey_connectivity_layer="INK1", structure_smiles_no_stereo="C", structure_exact_mass="100", structure_xlogp="1", structure_molecular_formula="C")
  tidytable::fwrite(met, file="met.tsv")
  nam <- tidytable::tidytable(structure_inchikey_connectivity_layer="INK1", structure_smiles_no_stereo="C", structure_name="Name1")
  tidytable::fwrite(nam, file="nam.tsv")
  cla <- tidytable::tidytable(structure_inchikey_connectivity_layer="INK1", structure_smiles_no_stereo="C", structure_tax_cla_chemontid="X", structure_tax_cla_01kin="Kin", structure_tax_cla_02sup="Sup", structure_tax_cla_03cla="Cla", structure_tax_cla_04dirpar="Par")
  tidytable::fwrite(cla, file="cla.tsv")
  npc <- tidytable::tidytable(structure_smiles_no_stereo="C", structure_tax_npc_01pat="Pat", structure_tax_npc_02sup="NSup", structure_tax_npc_03cla="NCla")
  tidytable::fwrite(npc, file="npc.tsv")
  result <- complement_metadata_structures(df,
    str_stereo="stereo.tsv", str_met="met.tsv", str_nam="nam.tsv", str_tax_cla="cla.tsv", str_tax_npc="npc.tsv")
  expect_true("candidate_structure_molecular_formula_i" %in% names(result) || "candidate_structure_molecular_formula_s" %in% names(result))
})
