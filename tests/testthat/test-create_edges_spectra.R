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

# test_that("create_edges_spectra handles empty MGF file", {
#   mgf_file <- tempfile(fileext = ".mgf")
#   writeLines("", mgf_file)
#
#   # Should handle gracefully
#   expect_error(
#     create_edges_spectra(input = mgf_file),
#     NA # Any specific error is acceptable here
#   )
# })

test_that("create_edges_spectra handles single spectrum", {
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
  mgf_file <- tempfile(fileext = ".mgf")
  mgf <- c(
    "BEGIN IONS",
    "TITLE=Spectrum1",
    "PEPMASS=100",
    "CHARGE=1+",
    "50 10",
    "75 20",
    "100 30",
    "END IONS",
    "",
    "BEGIN IONS",
    "TITLE=Spectrum2",
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

# test_that("create_edges_spectra returns output path", {
#   mgf_file <- tempfile(fileext = ".mgf")
#   mgf <- c(
#     "BEGIN IONS",
#     "PEPMASS=100",
#     "CHARGE=1+",
#     "100 10",
#     "END IONS"
#   )
#   writeLines(mgf, mgf_file)
#
#   output <- create_edges_spectra(
#     input = mgf_file,
#     threshold = 0.5
#   )
#
#   expect_type(output, "character")
#   expect_true(length(output) == 1)
# })
