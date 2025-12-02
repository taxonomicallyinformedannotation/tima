# Test Suite: prepare_annotations_sirius ----

library(testthat)

## Internal Utility Helpers ----

#' Stage structure fixtures for SIRIUS annotation tests
#'
#' @param root Root directory for staging
#' @return Named list of structure file paths
stage_structure_fixtures <- function(
  root = temp_test_dir("prep_sirius_structures")
) {
  # Create directory structure
  dir.create(
    file.path(root, "structures", "taxonomies"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  # Define target paths
  stereo <- file.path(root, "structures", "stereo.tsv")
  met <- file.path(root, "structures", "metadata.tsv")
  nam <- file.path(root, "structures", "names.tsv")
  cla <- file.path(root, "structures", "taxonomies", "classyfire.tsv")
  npc <- file.path(root, "structures", "taxonomies", "npc.tsv")

  # Copy fixtures using the helper that handles path resolution
  copy_fixture_to("structures_stereo.csv", stereo)
  copy_fixture_to("structures_metadata.csv", met)
  copy_fixture_to("structures_names.csv", nam)
  copy_fixture_to("structures_taxonomy_cla.csv", cla)
  copy_fixture_to("structures_taxonomy_npc.csv", npc)

  list(
    root = root,
    stereo = stereo,
    met = met,
    nam = nam,
    cla = cla,
    npc = npc
  )
}

## Validation ----

test_that("test-prepare_annotations_sirius validates sirius_version", {
  s <- stage_structure_fixtures()
  tmp <- tempdir()
  out_ann <- file.path(tmp, "ann.tsv")
  out_can <- file.path(tmp, "can.tsv")
  out_for <- file.path(tmp, "for.tsv")
  expect_error(
    prepare_annotations_sirius(
      input_directory = temp_test_path("missing.zip"),
      output_ann = out_ann,
      output_can = out_can,
      output_for = out_for,
      sirius_version = "7",
      str_stereo = s$stereo,
      str_met = s$met,
      str_nam = s$nam,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc
    ),
    "sirius_version must be '5' or '6'"
  )
})

test_that("test-prepare_annotations_sirius validates output parameters and structure files", {
  s <- stage_structure_fixtures()
  expect_error(
    prepare_annotations_sirius(
      input_directory = temp_test_path("missing.zip"),
      output_ann = c("a", "b"),
      output_can = "can.tsv",
      output_for = "for.tsv",
      sirius_version = "5",
      str_stereo = s$stereo,
      str_met = s$met,
      str_nam = s$nam,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc
    ),
    "Output path(s) must be single character strings: output_ann",
    fixed = TRUE
  )
  expect_error(
    prepare_annotations_sirius(
      input_directory = temp_test_path("missing.zip"),
      output_ann = "ann.tsv",
      output_can = "can.tsv",
      output_for = "for.tsv",
      sirius_version = "5",
      str_stereo = temp_test_path("missing.tsv"),
      str_met = s$met,
      str_nam = s$nam,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc
    ),
    "Required file(s) not found:",
    fixed = TRUE
  )
})

## Behavior ----

test_that("test-prepare_annotations_sirius handles missing input by producing empty outputs", {
  s <- stage_structure_fixtures()
  tmp <- tempdir()
  out_ann <- file.path(tmp, "ann.tsv")
  out_can <- file.path(tmp, "can.tsv")
  out_for <- file.path(tmp, "for.tsv")
  res <- prepare_annotations_sirius(
    input_directory = temp_test_path("not_there.zip"),
    output_ann = out_ann,
    output_can = out_can,
    output_for = out_for,
    sirius_version = "5",
    str_stereo = s$stereo,
    str_met = s$met,
    str_nam = s$nam,
    str_tax_cla = s$cla,
    str_tax_npc = s$npc
  )
  expect_true(file.exists(out_can))
  expect_true(file.exists(out_for))
  expect_true(file.exists(out_ann))
  expect_true(is.character(res[1]))
})

test_that("test-prepare_annotations_sirius handles version 6", {
  s <- stage_structure_fixtures()
  tmp <- tempdir()
  out_ann <- file.path(tmp, "ann_v6.tsv")
  out_can <- file.path(tmp, "can_v6.tsv")
  out_for <- file.path(tmp, "for_v6.tsv")
  res <- prepare_annotations_sirius(
    input_directory = temp_test_path("not_there.zip"),
    output_ann = out_ann,
    output_can = out_can,
    output_for = out_for,
    sirius_version = "6",
    str_stereo = s$stereo,
    str_met = s$met,
    str_nam = s$nam,
    str_tax_cla = s$cla,
    str_tax_npc = s$npc
  )
  expect_true(file.exists(out_can))
  expect_true(file.exists(out_for))
  expect_true(file.exists(out_ann))
})

test_that("test-prepare_annotations_sirius validates numeric version", {
  s <- stage_structure_fixtures()
  tmp <- tempdir()
  out_ann <- file.path(tmp, "ann_numeric.tsv")
  out_can <- file.path(tmp, "can_numeric.tsv")
  out_for <- file.path(tmp, "for_numeric.tsv")

  # Numeric versions should be converted and accepted
  expect_no_error(
    prepare_annotations_sirius(
      input_directory = temp_test_path("not_there.zip"),
      output_ann = out_ann,
      output_can = out_can,
      output_for = out_for,
      sirius_version = 5,
      str_stereo = s$stereo,
      str_met = s$met,
      str_nam = s$nam,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc
    )
  )
})

## Internal Helper Tests ----

test_that("validate_sirius_inputs validates version correctly", {
  s <- stage_structure_fixtures()
  tmp <- tempdir()

  expect_error(
    validate_sirius_inputs(
      sirius_version = "4",
      output_ann = file.path(tmp, "ann.tsv"),
      output_can = file.path(tmp, "can.tsv"),
      output_for = file.path(tmp, "for.tsv"),
      str_stereo = s$stereo,
      str_met = s$met,
      str_nam = s$nam,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc
    ),
    "sirius_version must be '5' or '6'"
  )

  # Test invalid type
  expect_error(
    validate_sirius_inputs(
      sirius_version = "invalid",
      output_ann = file.path(tmp, "ann.tsv"),
      output_can = file.path(tmp, "can.tsv"),
      output_for = file.path(tmp, "for.tsv"),
      str_stereo = s$stereo,
      str_met = s$met,
      str_nam = s$nam,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc
    ),
    "sirius_version must be '5' or '6'"
  )
})

test_that("validate_sirius_inputs validates output paths", {
  s <- stage_structure_fixtures()

  expect_error(
    validate_sirius_inputs(
      sirius_version = "5",
      output_ann = c("path1", "path2"),
      output_can = "can.tsv",
      output_for = "for.tsv",
      str_stereo = s$stereo,
      str_met = s$met,
      str_nam = s$nam,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc
    ),
    "Output path(s) must be single character strings: output_ann",
    fixed = TRUE
  )

  expect_error(
    validate_sirius_inputs(
      sirius_version = "5",
      output_ann = "ann.tsv",
      output_can = 123,
      output_for = "for.tsv",
      str_stereo = s$stereo,
      str_met = s$met,
      str_nam = s$nam,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc
    ),
    "Output path(s) must be single character strings: output_can",
    fixed = TRUE
  )

  expect_error(
    validate_sirius_inputs(
      sirius_version = "5",
      output_ann = "ann.tsv",
      output_can = "can.tsv",
      output_for = NULL,
      str_stereo = s$stereo,
      str_met = s$met,
      str_nam = s$nam,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc
    ),
    "Output path(s) must be single character strings: output_for",
    fixed = TRUE
  )
})

test_that("validate_sirius_inputs checks structure file existence", {
  s <- stage_structure_fixtures()
  tmp <- tempdir()

  expect_error(
    validate_sirius_inputs(
      sirius_version = "5",
      output_ann = file.path(tmp, "ann.tsv"),
      output_can = file.path(tmp, "can.tsv"),
      output_for = file.path(tmp, "for.tsv"),
      str_stereo = "nonexistent_stereo.tsv",
      str_met = s$met,
      str_nam = s$nam,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc
    ),
    "file.*not found"
  )

  expect_error(
    validate_sirius_inputs(
      sirius_version = "5",
      output_ann = file.path(tmp, "ann.tsv"),
      output_can = file.path(tmp, "can.tsv"),
      output_for = file.path(tmp, "for.tsv"),
      str_stereo = s$stereo,
      str_met = "nonexistent_metadata.tsv",
      str_nam = s$nam,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc
    ),
    "file.*not found"
  )
})

test_that("get_sirius_filenames returns correct names for version 5", {
  result <- get_sirius_filenames("5")

  expect_true(is.list(result))
  expect_equal(result$canopus, "canopus_compound_summary.tsv")
  expect_equal(result$formulas, "formula_identifications_all.tsv")
  expect_equal(result$structures, "compound_identifications_all.tsv")
  expect_null(result$denovo)
  expect_null(result$spectral)
})

test_that("get_sirius_filenames returns correct names for version 6", {
  result <- get_sirius_filenames("6")

  expect_true(is.list(result))
  expect_equal(result$canopus, "canopus_formula_summary_all.tsv")
  expect_equal(result$formulas, "formula_identifications_all.tsv")
  expect_equal(result$structures, "structure_identifications_all.tsv")
  expect_equal(result$denovo, "denovo_structure_identifications_all.tsv")
  expect_equal(result$spectral, "spectral_matches_all.tsv")
})

test_that("create_empty_sirius_annotations returns proper template", {
  skip_if_not_installed("tidytable")

  result <- create_empty_sirius_annotations()

  expect_true(tidytable::is_tidytable(result))
  expect_true(nrow(result) >= 0)

  # Check for SIRIUS-specific columns
  expected_cols <- c(
    "feature_pred_tax_cla_02sup_val",
    "feature_pred_tax_cla_03cla_val",
    "feature_pred_tax_npc_01pat_val",
    "candidate_score_sirius_sirius",
    "candidate_score_sirius_zodiac"
  )

  for (col in expected_cols) {
    expect_true(col %in% names(result))
  }
})

test_that("split_sirius_results splits table correctly", {
  # Create mock combined table
  mock_table <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    feature_pred_tax_cla_01kin_val = c("Kingdom1", NA, "Kingdom3"),
    feature_pred_tax_cla_02sup_val = c(
      "Superclass1",
      "Superclass2",
      "Superclass2"
    ),
    candidate_adduct = c("[M+H]+", "[M+Na]+", "[M+H]+"),
    candidate_score_sirius_sirius = c(0.9, 0.8, 0.7),
    candidate_structure_inchikey = c("KEY1", "KEY2", "KEY3"),
    candidate_count_sirius_peaks_explained = c(1, 2, 3)
  )

  result <- split_sirius_results(mock_table)

  expect_true(is.list(result))
  expect_true("canopus" %in% names(result))
  expect_true("formula" %in% names(result))
  expect_true("structures" %in% names(result))

  expect_true(tidytable::is_tidytable(result$canopus))
  expect_true(tidytable::is_tidytable(result$formula))
  expect_true(tidytable::is_tidytable(result$structures))
})
