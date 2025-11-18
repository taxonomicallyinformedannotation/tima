# ==============================================================================
# Test Suite: prepare_annotations_sirius - Input Validation
# ==============================================================================

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

# ==============================================================================
# Test Suite: prepare_annotations_sirius - Edge Cases
# ==============================================================================

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
