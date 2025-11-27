# Test Suite: prepare_libraries_spectra ----

library(testthat)

test_that("prepare_libraries_spectra() validates nam_lib parameter", {
  skip_if_not_installed("Spectra")

  tmpfile <- tempfile(fileext = ".mgf")
  # Create a minimal valid MGF file
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=Spectrum1",
      "PEPMASS=100.0",
      "50.0 100",
      "60.0 200",
      "END IONS"
    ),
    tmpfile
  )

  expect_error(
    prepare_libraries_spectra(
      input = tmpfile,
      nam_lib = c("lib1", "lib2") # Must be single string
    )
  )
})

test_that("prepare_libraries_spectra() validates column name parameters", {
  skip_if_not_installed("Spectra")

  tmpfile <- tempfile(fileext = ".mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=Spectrum1",
      "PEPMASS=100.0",
      "50.0 100",
      "END IONS"
    ),
    tmpfile
  )

  expect_error(
    prepare_libraries_spectra(
      input = tmpfile,
      col_ad = c("col1", "col2") # Must be single string
    )
  )
})
