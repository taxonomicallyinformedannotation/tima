# Test: Library Preparation Functions
library(testthat)
library(tima)

test_that("prepare_libraries_spectra handles non-existent input", {
  skip("Integration test - requires package structure")
})

test_that("prepare_libraries_spectra handles NULL input", {
  skip("Integration test - requires package structure")
})

test_that("prepare_libraries_spectra works with default parameters", {
  skip_on_cran()
  local_test_project(copy = TRUE)
  paths <- get_default_paths()

  # Download required files
  get_file(
    url = paths$urls$examples$spectral_lib_mini$pos,
    export = paths$data$interim$libraries$spectra$is$pos$isdb
  )
  get_file(
    url = paths$urls$examples$spectral_lib_mini$neg,
    export = paths$data$interim$libraries$spectra$is$neg$isdb
  )

  expect_no_error(prepare_libraries_spectra())
})

# New fast validation tests for prepare_libraries_rt

test_that("prepare_libraries_rt validates RT unit and outputs", {
  # Invalid unit
  expect_error(
    prepare_libraries_rt(unit_rt = "hours"),
    "must be 'seconds' or 'minutes'"
  )

  # Output paths must be single strings
  expect_error(prepare_libraries_rt(output_rt = c("a", "b")), "single")
  expect_error(prepare_libraries_rt(output_sop = c("a", "b")), "single")
})

# test_that("prepare_libraries_spectra warns when library already exists", {
#   skip_on_cran()
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$spectral_lib_mini$pos,
#     export = paths$data$interim$libraries$spectra$is$pos$isdb
#   )
#   get_file(
#     url = paths$urls$examples$spectral_lib_mini$neg,
#     export = paths$data$interim$libraries$spectra$is$neg$isdb
#   )
#
#   # First run
#   prepare_libraries_spectra()
#
#   # Second run should warn
#   expect_message(
#     prepare_libraries_spectra(),
#     regexp = "already"
#   )
# })

# test_that("prepare_libraries_rt works with experimental data", {
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$spectral_lib_mini$with_rt,
#     export = paths$data$source$libraries$spectra$exp$with_rt
#   )
#   get_file(
#     url = paths$urls$examples$lib_mini$rt,
#     export = paths$data$source$libraries$rt$example_mini
#   )
#
#   expect_no_error(
#     prepare_libraries_rt(
#       mgf_exp = list(pos = paths$data$source$libraries$spectra$exp$with_rt),
#       temp_exp = paths$data$source$libraries$rt$example_mini
#     )
#   )
# })

# test_that("prepare_libraries_rt works with in-silico data", {
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$spectral_lib_mini$with_rt,
#     export = paths$data$source$libraries$spectra$exp$with_rt
#   )
#   get_file(
#     url = paths$urls$examples$lib_mini$rt,
#     export = paths$data$source$libraries$rt$example_mini
#   )
#
#   expect_no_error(
#     prepare_libraries_rt(
#       mgf_is = list(pos = paths$data$source$libraries$spectra$exp$with_rt),
#       temp_is = paths$data$source$libraries$rt$example_mini
#     )
#   )
# })

# test_that("prepare_libraries_rt warns on invalid SMILES", {
#   local_test_project(copy = TRUE)
#
#   tidytable::tidytable(
#     "rt" = 0.1,
#     "smiles" = "wrongSMILES",
#     "inchikey" = NA
#   ) |>
#     tidytable::fwrite("data/source/libraries/rt/example_bad.tsv")
#
#   expect_warning(
#     prepare_libraries_rt(
#       temp_exp = "data/source/libraries/rt/example_bad.tsv"
#     )
#   )
# })

test_that("prepare_libraries_sop_closed works with and without input", {
  skip("Integration test - requires package structure")
})

test_that("prepare_libraries_sop_ecmdb handles missing input", {
  skip("Integration test - requires package structure")
})

test_that("prepare_libraries_sop_hmdb handles missing input", {
  skip("Integration test - requires package structure")
})

test_that("prepare_libraries_sop_lotus handles missing input", {
  skip("Integration test - requires package structure")
})

# test_that("prepare_libraries_sop_merged works with filtering", {
#   skip_on_cran()
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   # Need LOTUS data
#   fake_lotus(export = paths$data$source$libraries$sop$lotus)
#   prepare_libraries_sop_lotus()
#
#   expect_no_error(
#     prepare_libraries_sop_merged(
#       files = get_params(
#         step = "prepare_libraries_sop_merged"
#       )$files$libraries$sop$prepared$lotus,
#       filter = TRUE,
#       level = "family",
#       value = "Simaroubaceae|Gentianaceae",
#       output_key = "data/interim/libraries/sop/merged/bitter.tsv.gz"
#     )
#   )
# })

# test_that("prepare_libraries_sop_merged triggers SMILES processing", {
#   skip_on_cran()
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   # Create fake data
#   fake_ecmdb(export = paths$data$source$libraries$sop$ecmdb)
#   fake_lotus(export = paths$data$source$libraries$sop$lotus)
#
#   prepare_libraries_sop_ecmdb()
#   prepare_libraries_sop_lotus()
#   prepare_libraries_sop_closed()
#
#   expect_no_error(
#     prepare_libraries_sop_merged(
#       files = c(
#         get_params(
#           step = "prepare_libraries_sop_merged"
#         )$files$libraries$sop$prepared$closed,
#         get_params(
#           step = "prepare_libraries_sop_merged"
#         )$files$libraries$sop$prepared$ecmdb,
#         get_params(
#           step = "prepare_libraries_sop_merged"
#         )$files$libraries$sop$prepared$lotus
#       )
#     )
#   )
# })

# test_that("prepare_libraries_spectra preserves precursorMz as precursor_mz", {
#   skip_on_cran()
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   # Download required files
#   get_file(
#     url = paths$urls$examples$spectral_lib_mini$pos,
#     export = paths$data$interim$libraries$spectra$is$pos$isdb
#   )
#   get_file(
#     url = paths$urls$examples$spectral_lib_mini$neg,
#     export = paths$data$interim$libraries$spectra$is$neg$isdb
#   )
#
#   outputs <- prepare_libraries_spectra()
#   expect_true(file.exists(outputs[["pos"]]))
#
#   sp <- readRDS(outputs[["pos"]])
#   df <- Spectra::peaksDataFrame(sp)
#   # Ensure alias column exists at least at SOP generation stage
#   # Some backends store precursorMz in metadata. We verify SOP table as well.
#   expect_true("precursorMz" %in% names(df) || "precursor_mz" %in% names(df))
# })
