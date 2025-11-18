# ==============================================================================
# Tests for prepare_annotations_sirius() - Comprehensive Test Suite
# ==============================================================================

# Helper to create minimal structure files
create_dummy_structure_files <- function(dir) {
  str_stereo <- file.path(dir, "stereo.tsv")
  str_met <- file.path(dir, "metadata.tsv")
  str_nam <- file.path(dir, "names.tsv")
  str_tax_cla <- file.path(dir, "tax_cla.tsv")
  str_tax_npc <- file.path(dir, "tax_npc.tsv")

  # Create minimal valid files
  writeLines("structure_inchikey\tstructure_smiles", str_stereo)
  writeLines("structure_inchikey\tstructure_molecular_formula", str_met)
  writeLines("structure_inchikey\tstructure_name", str_nam)
  writeLines(
    "structure_inchikey_connectivity_layer\tstructure_tax_cla_chemontid",
    str_tax_cla
  )
  writeLines("structure_smiles_no_stereo\tstructure_tax_npc_01pat", str_tax_npc)

  list(
    str_stereo = str_stereo,
    str_met = str_met,
    str_nam = str_nam,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc
  )
}

# ==============================================================================
# Parameter Validation Tests
# ==============================================================================

test_that("prepare_annotations_sirius validates sirius_version", {
  tmp <- withr::local_tempdir()
  files <- create_dummy_structure_files(tmp)

  expect_error(
    prepare_annotations_sirius(
      input_directory = tempfile(),
      sirius_version = 4,
      str_stereo = files$str_stereo,
      str_met = files$str_met,
      str_nam = files$str_nam,
      str_tax_cla = files$str_tax_cla,
      str_tax_npc = files$str_tax_npc,
      output_ann = file.path(tmp, "ann.tsv"),
      output_can = file.path(tmp, "can.tsv"),
      output_for = file.path(tmp, "for.tsv")
    ),
    "must be '5' or '6'"
  )

  expect_error(
    prepare_annotations_sirius(
      input_directory = tempfile(),
      sirius_version = "v7",
      str_stereo = files$str_stereo,
      str_met = files$str_met,
      str_nam = files$str_nam,
      str_tax_cla = files$str_tax_cla,
      str_tax_npc = files$str_tax_npc,
      output_ann = file.path(tmp, "ann.tsv"),
      output_can = file.path(tmp, "can.tsv"),
      output_for = file.path(tmp, "for.tsv")
    ),
    "must be '5' or '6'"
  )
})

test_that("prepare_annotations_sirius validates output paths", {
  tmp <- withr::local_tempdir()
  files <- create_dummy_structure_files(tmp)

  # Non-character output
  expect_error(
    prepare_annotations_sirius(
      input_directory = tempfile(),
      sirius_version = "5",
      str_stereo = files$str_stereo,
      str_met = files$str_met,
      str_nam = files$str_nam,
      str_tax_cla = files$str_tax_cla,
      str_tax_npc = files$str_tax_npc,
      output_ann = 123,
      output_can = file.path(tmp, "can.tsv"),
      output_for = file.path(tmp, "for.tsv")
    ),
    "must be a single character string"
  )

  # Vector output
  expect_error(
    prepare_annotations_sirius(
      input_directory = tempfile(),
      sirius_version = "5",
      str_stereo = files$str_stereo,
      str_met = files$str_met,
      str_nam = files$str_nam,
      str_tax_cla = files$str_tax_cla,
      str_tax_npc = files$str_tax_npc,
      output_ann = file.path(tmp, "ann.tsv"),
      output_can = c("can1.tsv", "can2.tsv"),
      output_for = file.path(tmp, "for.tsv")
    ),
    "must be a single character string"
  )
})

test_that("prepare_annotations_sirius validates structure file paths", {
  tmp <- withr::local_tempdir()
  files <- create_dummy_structure_files(tmp)

  # Non-existent file
  expect_error(
    prepare_annotations_sirius(
      input_directory = tempfile(),
      sirius_version = "5",
      str_stereo = file.path(tmp, "nonexistent.tsv"),
      str_met = files$str_met,
      str_nam = files$str_nam,
      str_tax_cla = files$str_tax_cla,
      str_tax_npc = files$str_tax_npc,
      output_ann = file.path(tmp, "ann.tsv"),
      output_can = file.path(tmp, "can.tsv"),
      output_for = file.path(tmp, "for.tsv")
    ),
    "not found"
  )

  # Non-character path
  expect_error(
    prepare_annotations_sirius(
      input_directory = tempfile(),
      sirius_version = "5",
      str_stereo = files$str_stereo,
      str_met = 123,
      str_nam = files$str_nam,
      str_tax_cla = files$str_tax_cla,
      str_tax_npc = files$str_tax_npc,
      output_ann = file.path(tmp, "ann.tsv"),
      output_can = file.path(tmp, "can.tsv"),
      output_for = file.path(tmp, "for.tsv")
    ),
    "must be a single character string"
  )
})

# ==============================================================================
# Functional Tests
# ==============================================================================

test_that("prepare_annotations_sirius handles missing input directory", {
  tmp <- withr::local_tempdir()
  files <- create_dummy_structure_files(tmp)

  # Should return empty tables when input doesn't exist
  result <- prepare_annotations_sirius(
    input_directory = file.path(tmp, "nonexistent_sirius.zip"),
    sirius_version = "6",
    str_stereo = files$str_stereo,
    str_met = files$str_met,
    str_nam = files$str_nam,
    str_tax_cla = files$str_tax_cla,
    str_tax_npc = files$str_tax_npc,
    output_ann = file.path(tmp, "ann.tsv"),
    output_can = file.path(tmp, "can.tsv"),
    output_for = file.path(tmp, "for.tsv")
  )

  expect_type(result, "character")
  expect_named(result, c("canopus", "formula", "structural"))

  # Files should be created (even if empty)
  expect_true(file.exists(result["canopus"]))
  expect_true(file.exists(result["formula"]))
  expect_true(file.exists(result["structural"]))
})

test_that("prepare_annotations_sirius handles NULL input directory", {
  tmp <- withr::local_tempdir()
  files <- create_dummy_structure_files(tmp)

  # NULL input should be handled gracefully
  result <- prepare_annotations_sirius(
    input_directory = NULL,
    sirius_version = "5",
    str_stereo = files$str_stereo,
    str_met = files$str_met,
    str_nam = files$str_nam,
    str_tax_cla = files$str_tax_cla,
    str_tax_npc = files$str_tax_npc,
    output_ann = file.path(tmp, "ann.tsv"),
    output_can = file.path(tmp, "can.tsv"),
    output_for = file.path(tmp, "for.tsv")
  )

  expect_type(result, "character")
  expect_length(result, 3)
})

test_that("prepare_annotations_sirius accepts both numeric and character version", {
  tmp <- withr::local_tempdir()
  files <- create_dummy_structure_files(tmp)

  # Numeric 5 should work
  expect_no_error(
    prepare_annotations_sirius(
      input_directory = NULL,
      sirius_version = 5,
      str_stereo = files$str_stereo,
      str_met = files$str_met,
      str_nam = files$str_nam,
      str_tax_cla = files$str_tax_cla,
      str_tax_npc = files$str_tax_npc,
      output_ann = file.path(tmp, "ann1.tsv"),
      output_can = file.path(tmp, "can1.tsv"),
      output_for = file.path(tmp, "for1.tsv")
    )
  )

  # Character "6" should work
  expect_no_error(
    prepare_annotations_sirius(
      input_directory = NULL,
      sirius_version = "6",
      str_stereo = files$str_stereo,
      str_met = files$str_met,
      str_nam = files$str_nam,
      str_tax_cla = files$str_tax_cla,
      str_tax_npc = files$str_tax_npc,
      output_ann = file.path(tmp, "ann2.tsv"),
      output_can = file.path(tmp, "can2.tsv"),
      output_for = file.path(tmp, "for2.tsv")
    )
  )
})

# ==============================================================================
# Edge Cases
# ==============================================================================

test_that("prepare_annotations_sirius creates output files", {
  tmp <- withr::local_tempdir()
  files <- create_dummy_structure_files(tmp)

  out_ann <- file.path(tmp, "output_ann.tsv")
  out_can <- file.path(tmp, "output_can.tsv")
  out_for <- file.path(tmp, "output_for.tsv")

  result <- prepare_annotations_sirius(
    input_directory = NULL,
    sirius_version = "6",
    str_stereo = files$str_stereo,
    str_met = files$str_met,
    str_nam = files$str_nam,
    str_tax_cla = files$str_tax_cla,
    str_tax_npc = files$str_tax_npc,
    output_ann = out_ann,
    output_can = out_can,
    output_for = out_for
  )

  # All three output files should exist
  expect_true(file.exists(out_ann))
  expect_true(file.exists(out_can))
  expect_true(file.exists(out_for))

  # Result should contain paths
  expect_true(grepl("output_can.tsv", result["canopus"]))
  expect_true(grepl("output_for.tsv", result["formula"]))
  expect_true(grepl("output_ann.tsv", result["structural"]))
})
