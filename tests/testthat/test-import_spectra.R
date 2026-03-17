# Test Suite: import_spectra ----

library(testthat)

test_that("import_spectra handles MGF files correctly", {
  local_test_project(copy = TRUE)

  # Create test MGF in a temp file
  temp_mgf <- withr::local_tempfile(fileext = ".mgf")

  data.frame(
    FEATURE_ID = c("FT001", "FT002", "FT003"),
    mz = c(list(123.4567, 234.5678, 345.6789)),
    precursorCharge = c(0L, 0L, 0L),
    MS_LEVEL = c(1L, 2L, 3L),
    PRECURSOR_MZ = c(123.4567, 234.5678, 345.6789),
    Spectrum_type = c("MS1", "MS2", "MS3")
  ) |>
    Spectra::Spectra() |>
    MsBackendMgf::export(
      backend = MsBackendMgf::MsBackendMgf(),
      file = temp_mgf
    )

  result <- import_spectra(file = temp_mgf)
  expect_s4_class(result, "Spectra")
})

test_that("import_spectra handles MSP files with failures", {
  # This tests spectrum 1 failure case
  msp_file <- dir(
    system.file("extdata", package = "MsBackendMsp"),
    full.names = TRUE,
    pattern = "msp$"
  )[8L]

  if (file.exists(msp_file)) {
    result <- import_spectra(file = msp_file)
    expect_s4_class(result, "Spectra")
  } else {
    skip("MSP test file not found")
  }
})

test_that("import_spectra validates file and polarity inputs", {
  expect_error(
    import_spectra(file = 123),
    "single character string",
    class = "tima_validation_error"
  )

  expect_error(
    import_spectra(file = tempfile(fileext = ".mgf"), polarity = "bad"),
    "polarity",
    class = "tima_validation_error"
  )
})

test_that("import_spectra validates tolerances and missing files", {
  expect_error(
    import_spectra(file = "missing_file.mgf", dalton = 0),
    "positive",
    class = "tima_validation_error"
  )

  expect_error(
    import_spectra(file = "missing_file.mgf"),
    "spectra file not found",
    class = "tima_validation_error"
  )
})
