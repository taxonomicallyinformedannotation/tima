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
    "output_ann must be a single character string",
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
    "file(.*) not found",
    fixed = FALSE
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
