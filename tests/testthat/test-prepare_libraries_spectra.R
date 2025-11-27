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
      "100.0 100",
      "150.0 100",
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

test_that("prepare_libraries_spectra() prepares a single library MGF successfully", {
  skip_if_not_installed("Spectra")
  withr::local_dir(temp_test_dir("prepare_libraries_spectra_1"))
  mgf <- tempfile(fileext = ".mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=Spec1",
      "PEPMASS=150.05",
      "CHARGE=1+",
      "IONMODE=pos",
      "EXACTMASS=149.05",
      "SPECTRUMID=CCMSLIB0123456789",
      "FILENAME=Compound1",
      "NAME=Compound1",
      "INCHIKEY=AAAAAA-BBBBBB-N",
      "SMILES=CCC",
      "50 200",
      "150 200",
      "250 200",
      "END IONS"
    ),
    mgf
  )
  res <- prepare_libraries_spectra(input = mgf, nam_lib = "TEST_LIB")
  expect_type(res, "character")
})

test_that("prepare_libraries_spectra() handles multiple input files", {
  skip_if_not_installed("Spectra")
  withr::local_dir(temp_test_dir("prepare_libraries_spectra_2"))
  mgf1 <- tempfile(fileext = ".mgf")
  mgf2 <- tempfile(fileext = ".mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=Spec1",
      "PEPMASS=150.05",
      "CHARGE=1+",
      "IONMODE=pos",
      "EXACTMASS=149.05",
      "SPECTRUMID=CCMSLIB0123456789",
      "FILENAME=Compound1",
      "NAME=Compound1",
      "INCHIKEY=AAAAAA-BBBBBB-N",
      "SMILES=CCC",
      "50 200",
      "150 200",
      "250 200",
      "END IONS"
    ),
    mgf1
  )
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=Spec2",
      "PEPMASS=150.05",
      "CHARGE=1+",
      "IONMODE=pos",
      "EXACTMASS=149.05",
      "SPECTRUMID=CCMSLIB0123456789",
      "FILENAME=Compound1",
      "NAME=Compound1",
      "INCHIKEY=AAAAAA-BBBBBB-N",
      "SMILES=CCC",
      "51 200",
      "151 200",
      "251 200",
      "END IONS"
    ),
    mgf2
  )
  res <- prepare_libraries_spectra(input = c(mgf1, mgf2), nam_lib = "MULTI")
  expect_type(res, "character")
})

test_that("prepare_libraries_spectra() runs when optional columns are absent", {
  skip_if_not_installed("Spectra")
  withr::local_dir(temp_test_dir("prepare_libraries_spectra_3"))
  mgf <- tempfile(fileext = ".mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=Spec1",
      "PEPMASS=150.05",
      "CHARGE=1+",
      "IONMODE=pos",
      "EXACTMASS=149.05",
      "SPECTRUMID=CCMSLIB0123456789",
      "FILENAME=Compound1",
      "NAME=Compound1",
      "INCHIKEY=AAAAAA-BBBBBB-N",
      "SMILES=CCC",
      "50 200",
      "150 200",
      "250 200",
      "END IONS"
    ),
    mgf
  )
  res <- prepare_libraries_spectra(input = mgf, nam_lib = "NO_META")
  expect_type(res, "character")
})
