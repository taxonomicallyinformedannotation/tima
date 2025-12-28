# Test Suite: create_edges_spectra ----

library(testthat)

test_that("create_edges_spectra validates input parameter", {
  expect_error(
    create_edges_spectra(input = NULL),
    "must be"
  )

  expect_error(
    create_edges_spectra(input = 123),
    "must be"
  )
})

test_that("create_edges_spectra validates threshold parameter", {
  mgf_file <- tempfile(fileext = ".mgf")
  mgf <- c("BEGIN IONS", "PEPMASS=100", "CHARGE=1+", "100 10", "END IONS")
  writeLines(mgf, mgf_file)

  expect_error(
    create_edges_spectra(input = mgf_file, threshold = -0.1),
    "between 0 and 1"
  )

  expect_error(
    create_edges_spectra(input = mgf_file, threshold = 1.5),
    "between 0 and 1"
  )
})

test_that("create_edges_spectra validates matched_peaks parameter", {
  mgf_file <- tempfile(fileext = ".mgf")
  mgf <- c("BEGIN IONS", "PEPMASS=100", "CHARGE=1+", "100 10", "END IONS")
  writeLines(mgf, mgf_file)

  expect_error(
    create_edges_spectra(input = mgf_file, matched_peaks = 0),
    "positive integer"
  )

  expect_error(
    create_edges_spectra(input = mgf_file, matched_peaks = -5),
    "positive integer"
  )
})

test_that("create_edges_spectra validates ppm parameter", {
  mgf_file <- tempfile(fileext = ".mgf")
  mgf <- c("BEGIN IONS", "PEPMASS=100", "CHARGE=1+", "100 10", "END IONS")
  writeLines(mgf, mgf_file)

  expect_error(
    create_edges_spectra(input = mgf_file, ppm = -10),
    "positive"
  )
})

test_that("create_edges_spectra validates dalton parameter", {
  mgf_file <- tempfile(fileext = ".mgf")
  mgf <- c("BEGIN IONS", "PEPMASS=100", "CHARGE=1+", "100 10", "END IONS")
  writeLines(mgf, mgf_file)

  expect_error(
    create_edges_spectra(input = mgf_file, dalton = -0.01),
    "positive"
  )
})

test_that("create_edges_spectra checks input file exists", {
  expect_error(
    create_edges_spectra(input = "nonexistent.mgf"),
    "not found|does not exist"
  )
})

test_that("create_edges_spectra handles single spectrum", {
  withr::local_dir(new = temp_test_dir("create_edges_spectra_single"))
  mgf_file <- tempfile(fileext = ".mgf")
  mgf <- c(
    "BEGIN IONS",
    "TITLE=Spectrum1",
    "PEPMASS=100.5",
    "CHARGE=1+",
    "100 10",
    "101 20",
    "END IONS"
  )
  writeLines(mgf, mgf_file)

  output <- create_edges_spectra(
    input = mgf_file,
    threshold = 0.0,
    matched_peaks = 1,
    ppm = 10,
    dalton = 0.01
  )

  expect_true(file.exists(output))

  df <- tidytable::fread(output)
  # Single spectrum should produce empty edges or single NA row
  expect_true(nrow(df) <= 1)
})

test_that("create_edges_spectra creates output file", {
  withr::local_dir(new = temp_test_dir("create_edges_spectra_output"))
  mgf_file <- tempfile(fileext = ".mgf")
  mgf <- c(
    "BEGIN IONS",
    "TITLE=Spectrum1",
    "FEATURE_ID=1",
    "PEPMASS=100",
    "CHARGE=1+",
    "50 10",
    "75 20",
    "100 30",
    "END IONS",
    "",
    "BEGIN IONS",
    "TITLE=Spectrum2",
    "FEATURE_ID=2",
    "PEPMASS=200",
    "CHARGE=1+",
    "50 15",
    "75 25",
    "200 35",
    "END IONS"
  )
  writeLines(mgf, mgf_file)

  output <- create_edges_spectra(
    input = mgf_file,
    threshold = 0.0,
    matched_peaks = 1
  )

  expect_true(file.exists(output))
  expect_match(output, "\\.tsv$")
})

test_that("create_edges_spectra() creates edges from two spectra with entropy", {
  skip_if_not_installed("Spectra")
  withr::local_dir(new = temp_test_dir("create_edges_spectra_entropy"))
  mgf_file <- tempfile(fileext = ".mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=Spectrum1",
      "FEATURE_ID=1",
      "PEPMASS=100",
      "CHARGE=1+",
      "50 10",
      "75 20",
      "100 30",
      "END IONS",
      "",
      "BEGIN IONS",
      "TITLE=Spectrum2",
      "FEATURE_ID=2",
      "PEPMASS=200",
      "CHARGE=1+",
      "50 15",
      "75 25",
      "200 35",
      "END IONS"
    ),
    mgf_file
  )
  out <- create_edges_spectra(
    input = mgf_file,
    method = "entropy",
    threshold = 0.0,
    matched_peaks = 1
  )
  expect_true(file.exists(out))
  df <- tidytable::fread(out)
  expect_true(
    nrow(df) <= 1 || all(c("CLUSTERID1", "CLUSTERID2") %in% names(df))
  )
})

test_that("create_edges_spectra() runs with cosine method", {
  skip_if_not_installed("Spectra")
  withr::local_dir(new = temp_test_dir("create_edges_spectra_cosine"))
  mgf_file <- tempfile(fileext = ".mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=Spectrum1",
      "FEATURE_ID=1",
      "PEPMASS=100",
      "CHARGE=1+",
      "50 10",
      "75 20",
      "100 30",
      "END IONS",
      "",
      "BEGIN IONS",
      "TITLE=Spectrum2",
      "FEATURE_ID=2",
      "PEPMASS=200",
      "CHARGE=1+",
      "50 15",
      "75 25",
      "200 35",
      "END IONS"
    ),
    mgf_file
  )
  out <- create_edges_spectra(
    input = mgf_file,
    method = "cosine",
    threshold = 0.0,
    matched_peaks = 1
  )
  expect_true(file.exists(out))
})

test_that("create_edges_spectra handles empty MGF file", {
  withr::local_dir(new = temp_test_dir("create_edges_empty"))
  mgf_file <- tempfile(fileext = ".mgf")
  writeLines("", mgf_file)

  # Should handle gracefully
  expect_error(
    create_edges_spectra(input = mgf_file),
    "Spectra': 'data' must be of a vector type, was 'NULL'"
  )
})

test_that("create_edges_spectra handles invalid MGF format", {
  withr::local_dir(new = temp_test_dir("create_edges_invalid"))
  mgf_file <- tempfile(fileext = ".mgf")
  writeLines("INVALID MGF CONTENT", mgf_file)

  expect_error(
    create_edges_spectra(input = mgf_file),
    "Spectra': 'data' must be of a vector type, was 'NULL'"
  )
})
