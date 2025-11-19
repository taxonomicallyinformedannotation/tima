# Test Suite: prepare_annotations_sirius ----

library(testthat)

## Internal Utility Helpers ----

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

## Input Validation ----

test_that("prepare_annotations_sirius rejects invalid SIRIUS version", {
  expect_error(
    prepare_annotations_sirius(
      input_directory = tempfile(),
      output_ann = tempfile(),
      output_can = tempfile(),
      output_for = tempfile(),
      sirius_version = "7",
      str_stereo = system.file("extdata", package = "tima"),
      str_met = system.file("extdata", package = "tima"),
      str_nam = system.file("extdata", package = "tima"),
      str_tax_cla = system.file("extdata", package = "tima"),
      str_tax_npc = system.file("extdata", package = "tima")
    ),
    "sirius_version must be '5' or '6'"
  )
})

test_that("prepare_annotations_sirius accepts numeric SIRIUS versions", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  # Create dummy structure files
  writeLines("", "stereo.tsv")
  writeLines("", "met.tsv")
  writeLines("", "nam.tsv")
  writeLines("", "tax_cla.tsv")
  writeLines("", "tax_npc.tsv")

  # Version 5 as numeric
  expect_error(
    prepare_annotations_sirius(
      input_directory = "nonexistent.zip",
      output_ann = "out1.tsv",
      output_can = "out2.tsv",
      output_for = "out3.tsv",
      sirius_version = 5,
      str_stereo = "stereo.tsv",
      str_met = "met.tsv",
      str_nam = "nam.tsv",
      str_tax_cla = "tax_cla.tsv",
      str_tax_npc = "tax_npc.tsv"
    ),
    NA # Should not error on version validation
  )

  # Version 6 as numeric
  expect_error(
    prepare_annotations_sirius(
      input_directory = "nonexistent.zip",
      output_ann = "out1.tsv",
      output_can = "out2.tsv",
      output_for = "out3.tsv",
      sirius_version = 6,
      str_stereo = "stereo.tsv",
      str_met = "met.tsv",
      str_nam = "nam.tsv",
      str_tax_cla = "tax_cla.tsv",
      str_tax_npc = "tax_npc.tsv"
    ),
    NA # Should not error on version validation
  )
})

test_that("prepare_annotations_sirius rejects non-character output paths", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  writeLines("", "stereo.tsv")
  writeLines("", "met.tsv")
  writeLines("", "nam.tsv")
  writeLines("", "tax_cla.tsv")
  writeLines("", "tax_npc.tsv")

  expect_error(
    prepare_annotations_sirius(
      input_directory = tempfile(),
      output_ann = c("out1.tsv", "out2.tsv"),
      output_can = "out.tsv",
      output_for = "out.tsv",
      sirius_version = "5",
      str_stereo = "stereo.tsv",
      str_met = "met.tsv",
      str_nam = "nam.tsv",
      str_tax_cla = "tax_cla.tsv",
      str_tax_npc = "tax_npc.tsv"
    ),
    "output_ann must be a single character string"
  )
})

test_that("prepare_annotations_sirius rejects missing structure files", {
  expect_error(
    prepare_annotations_sirius(
      input_directory = tempfile(),
      output_ann = tempfile(),
      output_can = tempfile(),
      output_for = tempfile(),
      sirius_version = "5",
      str_stereo = "nonexistent.tsv",
      str_met = "nonexistent.tsv",
      str_nam = "nonexistent.tsv",
      str_tax_cla = "nonexistent.tsv",
      str_tax_npc = "nonexistent.tsv"
    ),
    "file not found"
  )
})

test_that("prepare_annotations_sirius validates all output paths", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  writeLines("", "stereo.tsv")
  writeLines("", "met.tsv")
  writeLines("", "nam.tsv")
  writeLines("", "tax_cla.tsv")
  writeLines("", "tax_npc.tsv")

  # Test output_can
  expect_error(
    prepare_annotations_sirius(
      input_directory = tempfile(),
      output_ann = "out.tsv",
      output_can = c("out1.tsv", "out2.tsv"),
      output_for = "out.tsv",
      sirius_version = "5",
      str_stereo = "stereo.tsv",
      str_met = "met.tsv",
      str_nam = "nam.tsv",
      str_tax_cla = "tax_cla.tsv",
      str_tax_npc = "tax_npc.tsv"
    ),
    "output_can must be a single character string"
  )

  # Test output_for
  expect_error(
    prepare_annotations_sirius(
      input_directory = tempfile(),
      output_ann = "out.tsv",
      output_can = "out.tsv",
      output_for = 123,
      sirius_version = "5",
      str_stereo = "stereo.tsv",
      str_met = "met.tsv",
      str_nam = "nam.tsv",
      str_tax_cla = "tax_cla.tsv",
      str_tax_npc = "tax_npc.tsv"
    ),
    "output_for must be a single character string"
  )
})

test_that("prepare_annotations_sirius validates all structure file paths", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  writeLines("", "stereo.tsv")
  writeLines("", "met.tsv")
  writeLines("", "nam.tsv")
  writeLines("", "tax_cla.tsv")

  # Missing tax_npc
  expect_error(
    prepare_annotations_sirius(
      input_directory = tempfile(),
      output_ann = "out.tsv",
      output_can = "out.tsv",
      output_for = "out.tsv",
      sirius_version = "5",
      str_stereo = "stereo.tsv",
      str_met = "met.tsv",
      str_nam = "nam.tsv",
      str_tax_cla = "tax_cla.tsv",
      str_tax_npc = "nonexistent.tsv"
    ),
    "str_tax_npc file not found"
  )
})

test_that("prepare_annotations_sirius rejects non-character structure paths", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  writeLines("", "stereo.tsv")
  writeLines("", "met.tsv")
  writeLines("", "nam.tsv")
  writeLines("", "tax_cla.tsv")
  writeLines("", "tax_npc.tsv")

  expect_error(
    prepare_annotations_sirius(
      input_directory = tempfile(),
      output_ann = "out.tsv",
      output_can = "out.tsv",
      output_for = "out.tsv",
      sirius_version = "5",
      str_stereo = c("stereo1.tsv", "stereo2.tsv"),
      str_met = "met.tsv",
      str_nam = "nam.tsv",
      str_tax_cla = "tax_cla.tsv",
      str_tax_npc = "tax_npc.tsv"
    ),
    "str_stereo must be a single character string"
  )
})

test_that("prepare_annotations_sirius handles NULL input_directory gracefully", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  writeLines("", "stereo.tsv")
  writeLines("", "met.tsv")
  writeLines("", "nam.tsv")
  writeLines("", "tax_cla.tsv")
  writeLines("", "tax_npc.tsv")

  # Should convert NULL to "Th1sd1rw0nt3x1st" and not find it
  # This tests the NULL handling logic
  expect_no_error(
    prepare_annotations_sirius(
      input_directory = NULL,
      output_ann = "out.tsv",
      output_can = "out.tsv",
      output_for = "out.tsv",
      sirius_version = "5",
      str_stereo = "stereo.tsv",
      str_met = "met.tsv",
      str_nam = "nam.tsv",
      str_tax_cla = "tax_cla.tsv",
      str_tax_npc = "tax_npc.tsv"
    )
  )
})

## Edge Cases ----

test_that("prepare_annotations_sirius handles missing input directory gracefully", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  writeLines("", "stereo.tsv")
  writeLines("", "met.tsv")
  writeLines("", "nam.tsv")
  writeLines("", "tax_cla.tsv")
  writeLines("", "tax_npc.tsv")

  # Should handle missing directory without crashing
  result <- expect_no_error(
    prepare_annotations_sirius(
      input_directory = "nonexistent_dir.zip",
      output_ann = "out_ann.tsv",
      output_can = "out_can.tsv",
      output_for = "out_for.tsv",
      sirius_version = "5",
      str_stereo = "stereo.tsv",
      str_met = "met.tsv",
      str_nam = "nam.tsv",
      str_tax_cla = "tax_cla.tsv",
      str_tax_npc = "tax_npc.tsv"
    )
  )

  expect_type(result, "character")
})

test_that("prepare_annotations_sirius converts sirius_version to character", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  writeLines("", "stereo.tsv")
  writeLines("", "met.tsv")
  writeLines("", "nam.tsv")
  writeLines("", "tax_cla.tsv")
  writeLines("", "tax_npc.tsv")

  # Should accept numeric and convert to character
  expect_no_error(
    prepare_annotations_sirius(
      input_directory = "nonexistent.zip",
      output_ann = "out.tsv",
      output_can = "out.tsv",
      output_for = "out.tsv",
      sirius_version = 5, # numeric, not character
      str_stereo = "stereo.tsv",
      str_met = "met.tsv",
      str_nam = "nam.tsv",
      str_tax_cla = "tax_cla.tsv",
      str_tax_npc = "tax_npc.tsv"
    )
  )
})

test_that("prepare_annotations_sirius accepts valid string versions", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  writeLines("", "stereo.tsv")
  writeLines("", "met.tsv")
  writeLines("", "nam.tsv")
  writeLines("", "tax_cla.tsv")
  writeLines("", "tax_npc.tsv")

  # Version "5"
  expect_no_error(
    prepare_annotations_sirius(
      input_directory = "nonexistent.zip",
      output_ann = "out.tsv",
      output_can = "out.tsv",
      output_for = "out.tsv",
      sirius_version = "5",
      str_stereo = "stereo.tsv",
      str_met = "met.tsv",
      str_nam = "nam.tsv",
      str_tax_cla = "tax_cla.tsv",
      str_tax_npc = "tax_npc.tsv"
    )
  )

  # Version "6"
  expect_no_error(
    prepare_annotations_sirius(
      input_directory = "nonexistent.zip",
      output_ann = "out.tsv",
      output_can = "out.tsv",
      output_for = "out.tsv",
      sirius_version = "6",
      str_stereo = "stereo.tsv",
      str_met = "met.tsv",
      str_nam = "nam.tsv",
      str_tax_cla = "tax_cla.tsv",
      str_tax_npc = "tax_npc.tsv"
    )
  )
})


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

## Parameter Validation ----

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

# Functional Tests ----

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
