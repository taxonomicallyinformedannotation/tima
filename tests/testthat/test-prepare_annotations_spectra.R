# Test Suite: prepare_annotations_spectra ----

library(testthat)

## Internal Utility Helpers ----

make_min_struct_files_spectra <- function(tmp) {
  # Use tmp directory or create temp path
  if (is.null(tmp)) {
    tmp <- temp_test_dir("spectra_test")
  }

  dir.create(
    file.path(tmp, "structures", "taxonomies"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  stereo <- file.path(tmp, "structures", "stereo.tsv")
  met <- file.path(tmp, "structures", "metadata.tsv")
  nam <- file.path(tmp, "structures", "names.tsv")
  cla <- file.path(tmp, "structures", "taxonomies", "classyfire.tsv")
  npc <- file.path(tmp, "structures", "taxonomies", "npc.tsv")

  # Use fixtures instead of creating inline
  tidytable::fwrite(
    load_fixture("spectra_structures_stereo"),
    stereo,
    sep = "\t"
  )
  tidytable::fwrite(
    load_fixture("spectra_structures_metadata"),
    met,
    sep = "\t"
  )
  tidytable::fwrite(
    load_fixture("spectra_structures_names"),
    nam,
    sep = "\t"
  )
  tidytable::fwrite(
    load_fixture("spectra_structures_taxonomy_cla"),
    cla,
    sep = "\t"
  )
  tidytable::fwrite(
    load_fixture("spectra_structures_taxonomy_npc"),
    npc,
    sep = "\t"
  )
  list(stereo = stereo, met = met, nam = nam, cla = cla, npc = npc)
}

## Validation ----

test_that("test-prepare_annotations_spectra validates input vector and files", {
  s <- make_min_struct_files_spectra(NULL)
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
  s <- make_min_struct_files_spectra(NULL)
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
