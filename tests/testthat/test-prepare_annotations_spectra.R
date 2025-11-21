# Test Suite: prepare_annotations_spectra ----

library(testthat)

## Internal Utility Helpers ----

make_min_struct_files_spectra <- function(tmp) {
  dir.create(
    file.path("structures", "taxonomies"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  stereo <- file.path("structures", "stereo.tsv")
  met <- file.path("structures", "metadata.tsv")
  nam <- file.path("structures", "names.tsv")
  cla <- file.path("structures", "taxonomies", "classyfire.tsv")
  npc <- file.path("structures", "taxonomies", "npc.tsv")

  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey_connectivity_layer = "DUMMYINK-PLANAR",
      structure_smiles_no_stereo = "C"
    ),
    stereo,
    sep = "\t"
  )
  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey_connectivity_layer = "DUMMYINK-PLANAR",
      structure_smiles_no_stereo = "C",
      structure_exact_mass = "16.0313",
      structure_xlogp = "0.1",
      structure_molecular_formula = "CH4"
    ),
    met,
    sep = "\t"
  )
  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey_connectivity_layer = "DUMMYINK-PLANAR",
      structure_smiles_no_stereo = "C",
      structure_name = "Dummy"
    ),
    nam,
    sep = "\t"
  )
  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey_connectivity_layer = "DUMMYINK-PLANAR",
      structure_tax_cla_chemontid = NA_character_,
      structure_tax_cla_01kin = NA_character_,
      structure_tax_cla_02sup = NA_character_,
      structure_tax_cla_03cla = NA_character_,
      structure_tax_cla_04dirpar = NA_character_
    ),
    cla,
    sep = "\t"
  )
  tidytable::fwrite(
    tidytable::tidytable(
      structure_smiles_no_stereo = "C",
      structure_tax_npc_01pat = NA_character_,
      structure_tax_npc_02sup = NA_character_,
      structure_tax_npc_03cla = NA_character_
    ),
    npc,
    sep = "\t"
  )
  list(stereo = stereo, met = met, nam = nam, cla = cla, npc = npc)
}

## Validation ----

test_that("test-prepare_annotations_spectra validates input vector and files", {
  s <- make_min_struct_files_spectra(tmp)
  out <- temp_test_path("spectra.tsv")
  expect_error(
    prepare_annotations_spectra(
      input = 123,
      output = out,
      str_stereo = s$stereo,
      str_met = s$met,
      str_nam = s$nam,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc
    ),
    "non-empty character"
  )
  expect_error(prepare_annotations_spectra(
    input = c(temp_test_path("missing.tsv")),
    output = out,
    str_stereo = s$stereo,
    str_met = s$met,
    str_nam = s$nam,
    str_tax_cla = s$cla,
    str_tax_npc = s$npc
  ))
})

test_that("test-prepare_annotations_spectra validates structure files", {
  ann <- temp_test_path("ann.tsv")
  tidytable::fwrite(
    x = tidytable::tidytable(feature_id = "F1"),
    file = ann,
    sep = "\t"
  )
  out <- temp_test_path("spectra.tsv")
  expect_error(
    prepare_annotations_spectra(
      input = ann,
      output = out,
      str_stereo = temp_test_path("missing.tsv"),
      str_met = temp_test_path("missing.tsv"),
      str_nam = temp_test_path("missing.tsv"),
      str_tax_cla = temp_test_path("missing.tsv"),
      str_tax_npc = temp_test_path("missing.tsv")
    ),
    "Structure file\\(s\\) not found"
  )
})

## Behavior ----

test_that("test-prepare_annotations_spectra processes minimal formatted input", {
  s <- make_min_struct_files_spectra(tmp)
  out <- temp_test_path("spectra.tsv")

  tbl <- tidytable::tidytable(
    feature_id = c("F1"),
    candidate_adduct = c("[M+H]+"),
    candidate_library = c("lib"),
    candidate_spectrum_id = c("id1"),
    candidate_spectrum_entropy = c("1.0"),
    candidate_structure_error_mz = c("0.0"),
    candidate_structure_name = c("Cmpd"),
    candidate_structure_inchikey_connectivity_layer = c("AAAA-BBBB"),
    candidate_structure_smiles_no_stereo = c("C"),
    candidate_structure_molecular_formula = c("CH4"),
    candidate_structure_exact_mass = c("16.0313"),
    candidate_structure_xlogp = c("0.1"),
    candidate_score_similarity = c("0.9"),
    candidate_count_similarity_peaks_matched = c("10")
  )
  ann1 <- temp_test_path("ann1.tsv")
  tidytable::fwrite(x = tbl, file = ann1, sep = "\t")

  res <- prepare_annotations_spectra(
    input = c(ann1),
    output = out,
    str_stereo = s$stereo,
    str_met = s$met,
    str_nam = s$nam,
    str_tax_cla = s$cla,
    str_tax_npc = s$npc
  )
  expect_equal(res, out)
  expect_true(file.exists(out))
  df <- tidytable::fread(out)
  expect_true("feature_id" %in% names(df))
})
