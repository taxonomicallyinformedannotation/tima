# Test Suite: prepare_annotations_sirius_utils ----
# Tests for internal SIRIUS helper functions in prepare_annotations_sirius_utils.R

library(testthat)

# ── validate_sirius_inputs ────────────────────────────────────────────────────

test_that("validate_sirius_inputs returns NULL for valid inputs", {
  str_stereo <- tempfile(fileext = ".tsv")
  str_met <- tempfile(fileext = ".tsv")
  str_tax_cla <- tempfile(fileext = ".tsv")
  str_tax_npc <- tempfile(fileext = ".tsv")
  for (f in c(str_stereo, str_met, str_tax_cla, str_tax_npc)) {
    writeLines("col\nval", f)
  }
  on.exit(unlink(c(str_stereo, str_met, str_tax_cla, str_tax_npc)))

  result <- validate_sirius_inputs(
    sirius_version = "5",
    output_ann = tempfile(fileext = ".tsv"),
    output_can = tempfile(fileext = ".tsv"),
    output_for = tempfile(fileext = ".tsv"),
    str_stereo = str_stereo,
    str_met = str_met,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc,
    max_analog_abs_mz_error = 0.01
  )
  expect_null(result)
})

test_that("validate_sirius_inputs accepts version '6'", {
  str_stereo <- tempfile(fileext = ".tsv")
  str_met <- tempfile(fileext = ".tsv")
  str_tax_cla <- tempfile(fileext = ".tsv")
  str_tax_npc <- tempfile(fileext = ".tsv")
  for (f in c(str_stereo, str_met, str_tax_cla, str_tax_npc)) {
    writeLines("col\nval", f)
  }
  on.exit(unlink(c(str_stereo, str_met, str_tax_cla, str_tax_npc)))

  expect_null(validate_sirius_inputs(
    sirius_version = "6",
    output_ann = tempfile(),
    output_can = tempfile(),
    output_for = tempfile(),
    str_stereo = str_stereo,
    str_met = str_met,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc,
    max_analog_abs_mz_error = 0.02
  ))
})

test_that("validate_sirius_inputs accepts numeric version 5 or 6", {
  str_stereo <- tempfile(fileext = ".tsv")
  str_met <- tempfile(fileext = ".tsv")
  str_tax_cla <- tempfile(fileext = ".tsv")
  str_tax_npc <- tempfile(fileext = ".tsv")
  for (f in c(str_stereo, str_met, str_tax_cla, str_tax_npc)) {
    writeLines("col\nval", f)
  }
  on.exit(unlink(c(str_stereo, str_met, str_tax_cla, str_tax_npc)))

  expect_null(validate_sirius_inputs(
    sirius_version = 5,
    output_ann = tempfile(),
    output_can = tempfile(),
    output_for = tempfile(),
    str_stereo = str_stereo,
    str_met = str_met,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc,
    max_analog_abs_mz_error = 0.01
  ))
})

test_that("validate_sirius_inputs errors on invalid version", {
  str_stereo <- tempfile(fileext = ".tsv")
  str_met <- tempfile(fileext = ".tsv")
  str_tax_cla <- tempfile(fileext = ".tsv")
  str_tax_npc <- tempfile(fileext = ".tsv")
  for (f in c(str_stereo, str_met, str_tax_cla, str_tax_npc)) {
    writeLines("col\nval", f)
  }
  on.exit(unlink(c(str_stereo, str_met, str_tax_cla, str_tax_npc)))

  expect_error(
    validate_sirius_inputs(
      sirius_version = "4",
      output_ann = tempfile(),
      output_can = tempfile(),
      output_for = tempfile(),
      str_stereo = str_stereo,
      str_met = str_met,
      str_tax_cla = str_tax_cla,
      str_tax_npc = str_tax_npc,
      max_analog_abs_mz_error = 0.01
    ),
    class = "tima_error"
  )
})

test_that("validate_sirius_inputs errors when structure files do not exist", {
  expect_error(
    validate_sirius_inputs(
      sirius_version = "5",
      output_ann = tempfile(),
      output_can = tempfile(),
      output_for = tempfile(),
      str_stereo = "/nonexistent/stereo.tsv",
      str_met = "/nonexistent/met.tsv",
      str_tax_cla = "/nonexistent/cla.tsv",
      str_tax_npc = "/nonexistent/npc.tsv",
      max_analog_abs_mz_error = 0.01
    ),
    class = "tima_error"
  )
})

test_that("validate_sirius_inputs errors on negative max_analog_abs_mz_error", {
  str_stereo <- tempfile(fileext = ".tsv")
  str_met <- tempfile(fileext = ".tsv")
  str_tax_cla <- tempfile(fileext = ".tsv")
  str_tax_npc <- tempfile(fileext = ".tsv")
  for (f in c(str_stereo, str_met, str_tax_cla, str_tax_npc)) {
    writeLines("col\nval", f)
  }
  on.exit(unlink(c(str_stereo, str_met, str_tax_cla, str_tax_npc)))

  expect_error(
    validate_sirius_inputs(
      sirius_version = "5",
      output_ann = tempfile(),
      output_can = tempfile(),
      output_for = tempfile(),
      str_stereo = str_stereo,
      str_met = str_met,
      str_tax_cla = str_tax_cla,
      str_tax_npc = str_tax_npc,
      max_analog_abs_mz_error = -0.01
    ),
    class = "tima_error"
  )
})

# ── get_sirius_filenames ──────────────────────────────────────────────────────

test_that("get_sirius_filenames returns fixed filenames for version 5", {
  fnames <- get_sirius_filenames("5")
  expect_type(fnames, "list")
  expect_equal(fnames$canopus, "canopus_compound_summary.tsv")
  expect_equal(fnames$formulas, "formula_identifications_all.tsv")
  expect_equal(fnames$structures, "compound_identifications_all.tsv")
  expect_null(fnames$denovo)
  expect_null(fnames$spectral)
})

test_that("get_sirius_filenames returns regex patterns for version 6", {
  fnames <- get_sirius_filenames("6")
  expect_type(fnames, "list")
  expect_match(fnames$canopus, "canopus")
  expect_match(fnames$formulas, "formula_identifications")
  expect_match(fnames$structures, "structure_identifications")
  expect_match(fnames$denovo, "denovo")
  expect_match(fnames$spectral, "spectral_matches")
  # v6 patterns should be regex (contain special chars)
  expect_true(grepl("\\$", fnames$formulas, fixed = FALSE))
})

test_that("get_sirius_filenames names are consistent between versions", {
  v5 <- get_sirius_filenames("5")
  v6 <- get_sirius_filenames("6")
  expect_setequal(names(v5), names(v6))
})

# ── create_empty_sirius_annotations ──────────────────────────────────────────

test_that("create_empty_sirius_annotations returns a data frame with SIRIUS columns", {
  result <- create_empty_sirius_annotations()
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 1L)
  expect_true("candidate_score_sirius_csi" %in% colnames(result))
  expect_true("candidate_score_sirius_confidence" %in% colnames(result))
  expect_true("feature_pred_tax_npc_01pat_val" %in% colnames(result))
  expect_true("candidate_count_sirius_peaks_explained" %in% colnames(result))
})

# ── merge_sirius_structures_with_spectral ─────────────────────────────────────

test_that("merge_sirius_structures_with_spectral returns structures when spectral is empty", {
  structs <- tidytable::tidytable(
    feature_id = "f1",
    candidate_structure_smiles_no_stereo = "CC",
    candidate_score_sirius_csi = 0.8
  )
  spectral <- tidytable::tidytable(
    feature_id = character(),
    candidate_structure_smiles_no_stereo = character()
  )
  result <- merge_sirius_structures_with_spectral(structs, spectral)
  expect_equal(nrow(result), 1L)
  expect_equal(result$feature_id, "f1")
})

test_that("merge_sirius_structures_with_spectral returns spectral when structures is empty", {
  structs <- tidytable::tidytable(
    feature_id = character(),
    candidate_structure_smiles_no_stereo = character()
  )
  spectral <- tidytable::tidytable(
    feature_id = "f1",
    candidate_structure_smiles_no_stereo = "CC",
    candidate_score_similarity = 0.9
  )
  result <- merge_sirius_structures_with_spectral(structs, spectral)
  expect_equal(nrow(result), 1L)
  expect_equal(result$feature_id, "f1")
})

test_that("merge_sirius_structures_with_spectral merges overlapping rows", {
  structs <- tidytable::tidytable(
    feature_id = "f1",
    candidate_structure_smiles_no_stereo = "CC",
    candidate_score_sirius_csi = 0.8,
    candidate_library = "SIRIUS"
  )
  spectral <- tidytable::tidytable(
    feature_id = "f1",
    candidate_structure_smiles_no_stereo = "CC",
    candidate_score_similarity = 0.9,
    candidate_library = "spectral"
  )
  result <- merge_sirius_structures_with_spectral(structs, spectral)
  expect_true(nrow(result) >= 1L)
  expect_equal(result$feature_id[[1L]], "f1")
})
