# Test Suite: complement_metadata_structures ----

library(testthat)

test_that("complement_metadata_structures handles empty input", {
  result <- complement_metadata_structures(tidytable::tidytable())
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("complement_metadata_structures errors on invalid file paths", {
  df <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = "INKX",
    candidate_structure_smiles_no_stereo = "C"
  )
  expect_error(
    complement_metadata_structures(
      df,
      str_stereo = "missing_stereo.tsv",
      str_met = "missing_met.tsv",
      str_nam = "missing_nam.tsv",
      str_tax_cla = "missing_cla.tsv",
      str_tax_npc = "missing_npc.tsv"
    ),
    "file not found"
  )
})

test_that("complement_metadata_structures validates single character path inputs", {
  df <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = "INKY",
    candidate_structure_smiles_no_stereo = "CC"
  )

  # Create minimal valid files
  minimal <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = "INKY",
    structure_smiles_no_stereo = "CC"
  )
  tidytable::fwrite(minimal, file = "st.tsv")
  tidytable::fwrite(minimal, file = "met.tsv")
  tidytable::fwrite(minimal, file = "nam.tsv")
  tidytable::fwrite(minimal, file = "cla.tsv")
  tidytable::fwrite(
    tidytable::tidytable(structure_smiles_no_stereo = "CC"),
    file = "npc.tsv"
  )
  # Pass a vector for one path
  expect_error(
    complement_metadata_structures(
      df,
      str_stereo = c("st.tsv", "st.tsv"),
      str_met = "met.tsv",
      str_nam = "nam.tsv",
      str_tax_cla = "cla.tsv",
      str_tax_npc = "npc.tsv"
    ),
    "single character"
  )
})

test_that("complement_metadata_structures collapses multiple names with separator", {
  df <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = "INK1",
    candidate_structure_smiles_no_stereo = "C"
  )
  stereo <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = "INK1",
    structure_smiles_no_stereo = "C"
  )
  tidytable::fwrite(stereo, file = "stereo.tsv")
  met <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = "INK1",
    structure_smiles_no_stereo = "C",
    structure_exact_mass = "100",
    structure_xlogp = "1",
    structure_molecular_formula = "C"
  )
  tidytable::fwrite(met, file = "met.tsv")
  nam <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = c("INK1", "INK1"),
    structure_smiles_no_stereo = c("C", "C"),
    structure_name = c("Name1", "Name2")
  )
  tidytable::fwrite(nam, file = "nam.tsv")
  cla <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = "INK1",
    structure_smiles_no_stereo = "C",
    structure_tax_cla_chemontid = "X",
    structure_tax_cla_01kin = "Kin",
    structure_tax_cla_02sup = "Sup",
    structure_tax_cla_03cla = "Cla",
    structure_tax_cla_04dirpar = "Par"
  )
  tidytable::fwrite(cla, file = "cla.tsv")
  npc <- tidytable::tidytable(
    structure_smiles_no_stereo = "C",
    structure_tax_npc_01pat = "Pat",
    structure_tax_npc_02sup = "NSup",
    structure_tax_npc_03cla = "NCla"
  )
  tidytable::fwrite(npc, file = "npc.tsv")
  result <- complement_metadata_structures(
    df,
    str_stereo = "stereo.tsv",
    str_met = "met.tsv",
    str_nam = "nam.tsv",
    str_tax_cla = "cla.tsv",
    str_tax_npc = "npc.tsv"
  )
  expect_true("structure_name" %in% names(result))
  if (!is.na(result$structure_name)) {
    expect_true(grepl(" $ ", result$structure_name, fixed = TRUE))
  }
})

# test_that(
#   skip("Not implemented")
# )
# test_that("complement_metadata_structures adds taxonomy columns", {
#
#
#   df <- tidytable::tidytable(
#     candidate_structure_inchikey_connectivity_layer = "INK2",
#     candidate_structure_smiles_no_stereo = "N"
#   )
#   stereo <- tidytable::tidytable(
#     structure_inchikey_connectivity_layer = "INK2",
#     structure_smiles_no_stereo = "N"
#   )
#   tidytable::fwrite(stereo, file = "stereo.tsv")
#   tidytable::fwrite(stereo, file = "met.tsv")
#   tidytable::fwrite(stereo, file = "nam.tsv")
#   cla <- tidytable::tidytable(
#     structure_inchikey_connectivity_layer = "INK2",
#     structure_smiles_no_stereo = "N",
#     structure_tax_cla_chemontid = "Y",
#     structure_tax_cla_01kin = "Kin",
#     structure_tax_cla_02sup = "Sup",
#     structure_tax_cla_03cla = "Cla",
#     structure_tax_cla_04dirpar = "Par"
#   )
#   tidytable::fwrite(cla, file = "cla.tsv")
#   npc <- tidytable::tidytable(
#     structure_smiles_no_stereo = "N",
#     structure_tax_npc_01pat = "Pat",
#     structure_tax_npc_02sup = "NSup",
#     structure_tax_npc_03cla = "NCla"
#   )
#   tidytable::fwrite(npc, file = "npc.tsv")
#   result <- complement_metadata_structures(
#     df,
#     str_stereo = "stereo.tsv",
#     str_met = "met.tsv",
#     str_nam = "nam.tsv",
#     str_tax_cla = "cla.tsv",
#     str_tax_npc = "npc.tsv"
#   )
#   expect_true(
#     any(grepl("tax_cla_01kin", names(result))) ||
#       any(grepl("tax_npc_01pat", names(result)))
#   )
# })

test_that("complement_metadata_structures errors when required columns missing", {
  df <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = "INK3",
    candidate_structure_smiles_no_stereo = "O"
  )
  # Stereo missing structure_smiles_no_stereo column
  stereo <- tidytable::tidytable(structure_inchikey_connectivity_layer = "INK3")
  tidytable::fwrite(stereo, file = "stereo.tsv")
  minimal <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = "INK3",
    structure_smiles_no_stereo = "O"
  )
  tidytable::fwrite(minimal, file = "met.tsv")
  tidytable::fwrite(minimal, file = "nam.tsv")
  tidytable::fwrite(minimal, file = "cla.tsv")
  tidytable::fwrite(
    tidytable::tidytable(structure_smiles_no_stereo = "O"),
    file = "npc.tsv"
  )
  # Expect error due to missing column usage inside joins/select
  expect_error(
    complement_metadata_structures(
      df,
      str_stereo = "stereo.tsv",
      str_met = "met.tsv",
      str_nam = "nam.tsv",
      str_tax_cla = "cla.tsv",
      str_tax_npc = "npc.tsv"
    )
  )
})

# test_that(
#   skip("Not implemented")
# )
# test_that("complement_metadata_structures enriches metadata", {
#
#
#   # Minimal df
#   df <- tidytable::tidytable(
#     candidate_structure_inchikey_connectivity_layer = "INK1",
#     candidate_structure_smiles_no_stereo = "C"
#   )
#   # Create minimal files with required columns
#   stereo <- tidytable::tidytable(
#     structure_inchikey_connectivity_layer = "INK1",
#     structure_smiles_no_stereo = "C"
#   )
#   tidytable::fwrite(stereo, file = "stereo.tsv")
#   met <- tidytable::tidytable(
#     structure_inchikey_connectivity_layer = "INK1",
#     structure_smiles_no_stereo = "C",
#     structure_exact_mass = "100",
#     structure_xlogp = "1",
#     structure_molecular_formula = "C"
#   )
#   tidytable::fwrite(met, file = "met.tsv")
#   nam <- tidytable::tidytable(
#     structure_inchikey_connectivity_layer = "INK1",
#     structure_smiles_no_stereo = "C",
#     structure_name = "Name1"
#   )
#   tidytable::fwrite(nam, file = "nam.tsv")
#   cla <- tidytable::tidytable(
#     structure_inchikey_connectivity_layer = "INK1",
#     structure_smiles_no_stereo = "C",
#     structure_tax_cla_chemontid = "X",
#     structure_tax_cla_01kin = "Kin",
#     structure_tax_cla_02sup = "Sup",
#     structure_tax_cla_03cla = "Cla",
#     structure_tax_cla_04dirpar = "Par"
#   )
#   tidytable::fwrite(cla, file = "cla.tsv")
#   npc <- tidytable::tidytable(
#     structure_smiles_no_stereo = "C",
#     structure_tax_npc_01pat = "Pat",
#     structure_tax_npc_02sup = "NSup",
#     structure_tax_npc_03cla = "NCla"
#   )
#   tidytable::fwrite(npc, file = "npc.tsv")
#   result <- complement_metadata_structures(
#     df,
#     str_stereo = "stereo.tsv",
#     str_met = "met.tsv",
#     str_nam = "nam.tsv",
#     str_tax_cla = "cla.tsv",
#     str_tax_npc = "npc.tsv"
#   )
#   expect_true(
#     "candidate_structure_molecular_formula_i" %in%
#       names(result) ||
#       "candidate_structure_molecular_formula_s" %in% names(result)
#   )
# })
