# Test Suite: prepare_annotations_gnps ----

library(testthat)

## Validation ----

test_that("test-prepare_annotations_gnps validates output path", {
  # Stage fixtures BEFORE changing directory
  s <- stage_structure_fixtures("prep_gnps_structures")
  withr::local_dir(new = as.character(s$root))

  expect_error(
    prepare_annotations_gnps(
      input = character(0),
      output = c("a.tsv", "b.tsv"),
      str_stereo = s$stereo,
      str_met = s$met,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc
    ),
    "output must be a single character string",
    class = "tima_validation_error"
  )
})

test_that("test-prepare_annotations_gnps validates structure file paths", {
  out <- temp_test_path("gnps.tsv")

  expect_error(
    prepare_annotations_gnps(
      input = character(0),
      output = out,
      str_stereo = temp_test_path("missing.tsv"),
      str_met = temp_test_path("missing.tsv"),
      str_tax_cla = temp_test_path("missing.tsv"),
      str_tax_npc = temp_test_path("missing.tsv")
    ),
    "Required file(s) not found",
    fixed = TRUE
  )
})

# Behavior ----

test_that("test-prepare_annotations_gnps handles missing input files by creating empty output", {
  # Stage fixtures BEFORE changing directory
  s <- stage_structure_fixtures("prep_gnps_structures")
  withr::local_dir(new = as.character(s$root))

  out <- temp_test_path("gnps.tsv")
  res <- prepare_annotations_gnps(
    input = temp_test_path("does_not_exist.tsv"),
    output = out,
    str_stereo = s$stereo,
    str_met = s$met,
    str_tax_cla = s$cla,
    str_tax_npc = s$npc
  )
  expect_equal(res, out)
  expect_true(file.exists(out))
  df <- tidytable::fread(out)
  expect_true(is.data.frame(df))
})

test_that("test-prepare_annotations_gnps processes minimal valid GNPS file", {
  # Stage fixtures BEFORE changing directory
  s <- stage_structure_fixtures("prep_gnps_structures")
  withr::local_dir(new = as.character(s$root))
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
    str_tax_cla = s$cla,
    str_tax_npc = s$npc
  )
  expect_equal(res, out)
  expect_true(file.exists(out))
  df <- tidytable::fread(out)
  expect_true("feature_id" %in% names(df))
})
