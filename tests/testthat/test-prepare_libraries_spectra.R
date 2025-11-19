# Test Suite: prepare_libraries_spectra ----

library(testthat)

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
