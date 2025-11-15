# Test: Spectra Import and Processing
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

  result <- import_spectra(temp_mgf)
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
    result <- import_spectra(msp_file)
    expect_s4_class(result, "Spectra")
  } else {
    skip("MSP test file not found")
  }
})

test_that("sanitize_spectra handles FEATURE_ID column", {
  df <- data.frame(
    FEATURE_ID = c("FT001", "FT002", "FT003"),
    mz = c(list(123.4567, 234.5678, 345.6789))
  ) |>
    Spectra::Spectra() |>
    sanitize_spectra()

  expect_s4_class(df, "Spectra")
  expect_true("FEATURE_ID" %in% colnames(df@backend@spectraData))
})

test_that("sanitize_spectra handles SLAW_ID column", {
  df <- data.frame(
    SLAW_ID = c("FT001", "FT002", "FT003"),
    mz = c(list(123.4567, 234.5678, 345.6789))
  ) |>
    Spectra::Spectra() |>
    sanitize_spectra()

  expect_s4_class(df, "Spectra")
  expect_true("SLAW_ID" %in% colnames(df@backend@spectraData))
})

test_that("read_mgf_opti fails with multiple files", {
  expect_error(
    read_mgf_opti(f = c("foo", "bar"))
  )
})
