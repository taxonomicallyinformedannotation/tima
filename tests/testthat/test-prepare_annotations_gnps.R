# Test Suite: prepare_annotations_gnps ----

library(testthat)

## Internal Utility Helpers ----

make_min_struct_files <- function(root = getwd()) {
  dir.create(
    file.path(root, "structures", "taxonomies"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  stereo <- file.path(root, "structures", "stereo.tsv")
  met <- file.path(root, "structures", "metadata.tsv")
  nam <- file.path(root, "structures", "names.tsv")
  cla <- file.path(root, "structures", "taxonomies", "classyfire.tsv")
  npc <- file.path(root, "structures", "taxonomies", "npc.tsv")

  # Write minimal valid content (headers + one row) for each file
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

test_that("test-prepare_annotations_gnps validates output path", {
  withr::local_dir(new = temp_test_dir("prep_gnps_validate_output"))
  s <- make_min_struct_files()
  expect_error(
    prepare_annotations_gnps(
      input = character(0),
      output = c("a.tsv", "b.tsv"),
      str_stereo = s$stereo,
      str_met = s$met,
      str_nam = s$nam,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc
    ),
    "output must be a single character string"
  )
})

test_that("test-prepare_annotations_gnps validates structure file paths", {
  withr::local_dir(new = temp_test_dir("prep_gnps_validate_struct"))
  out <- temp_test_path("gnps.tsv")
  expect_error(
    prepare_annotations_gnps(
      input = character(0),
      output = out,
      str_stereo = temp_test_path("missing.tsv"),
      str_met = temp_test_path("missing.tsv"),
      str_nam = temp_test_path("missing.tsv"),
      str_tax_cla = temp_test_path("missing.tsv"),
      str_tax_npc = temp_test_path("missing.tsv")
    ),
    "file not found"
  )
})

# Behavior ----

test_that("test-prepare_annotations_gnps handles missing input files by creating empty output", {
  withr::local_dir(new = temp_test_dir("prep_gnps_missing_input"))
  s <- make_min_struct_files()
  out <- temp_test_path("gnps.tsv")
  res <- prepare_annotations_gnps(
    input = temp_test_path("does_not_exist.tsv"),
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
  expect_true(is.data.frame(df))
})

test_that("test-prepare_annotations_gnps processes minimal valid GNPS file", {
  withr::local_dir(new = temp_test_dir("prep_gnps_minimal"))
  s <- make_min_struct_files()
  out <- temp_test_path("gnps.tsv")
  gnps <- tidytable::tidytable(
    `#Scan#` = c("F1"),
    Adduct = c("[M+H]+"),
    MassDiff = c("0.0"),
    LibraryName = c("GNPS"),
    Compound_Name = c("Cmpd"),
    MQScore = c("0.9"),
    SharedPeaks = c("10"),
    INCHI = c("InChI=1S/C"),
    `InChIKey` = c("AAAAAAAAAAAAAA-BBBBBBBBBB-C"),
    `InChIKey-Planar` = c("AAAAAAAAAAAAAA-BBBBBBBBBB"),
    npclassifier_pathway = c("Path"),
    npclassifier_superclass = c("Sup"),
    npclassifier_class = c("Cla"),
    ExactMass = c("100.0"),
    superclass = c("Super"),
    class = c("Class"),
    subclass = c("Subclass"),
    Precursor_MZ = c("100.0"),
    MZErrorPPM = c("0")
  )
  gnps_path <- temp_test_path("gnps_in.tsv")
  tidytable::fwrite(x = gnps, file = gnps_path, sep = "\t")
  res <- prepare_annotations_gnps(
    input = gnps_path,
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
