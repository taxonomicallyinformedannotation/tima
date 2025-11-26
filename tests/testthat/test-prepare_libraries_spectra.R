# Test Suite: prepare_libraries_spectra ----

library(testthat)

test_that("prepare_libraries_spectra returns empty libs when input missing", {
  withr::local_dir(temp_test_dir("prep_lib_spectra_missing"))
  local_test_project(copy = TRUE)
  out <- prepare_libraries_spectra(
    input = c("missing1.mgf", "missing2.mgf"),
    nam_lib = "testlib"
  )
  expect_true(file.exists(out[["pos"]]))
  expect_true(file.exists(out[["neg"]]))
  expect_true(file.exists(out[["sop"]]))
  sp_pos <- readRDS(out[["pos"]])
  expect_s4_class(sp_pos, "Spectra")
  expect_equal(length(sp_pos), 1)
})
