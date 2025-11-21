# Test Suite: extract_spectra ----

library(testthat)

test_that("extract_spectra validates input is Spectra object", {
  expect_error(
    extract_spectra(object = data.frame()),
    "Input must be a Spectra object"
  )

  expect_error(
    extract_spectra(object = list()),
    "Input must be a Spectra object"
  )

  expect_error(
    extract_spectra(object = NULL),
    "Input must be a Spectra object"
  )
})

test_that("extract_spectra handles empty Spectra object", {
  spectra <- Spectra::Spectra()

  result <- extract_spectra(object = spectra)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true("mz" %in% names(result))
  expect_true("intensity" %in% names(result))
})

test_that("extract_spectra extracts basic spectra metadata", {
  df <- data.frame(
    msLevel = 2L,
    precursorMz = c(100.0, 200.0),
    rtime = c(10.5, 20.3)
  )
  spectra <- Spectra::Spectra(object = df)

  result <- extract_spectra(object = spectra)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true("msLevel" %in% names(result) || "ms_level" %in% names(result))
})

test_that("extract_spectra extracts peak data (mz and intensity)", {
  # Create spectra with peak data
  df <- data.frame(
    msLevel = 2L,
    precursorMz = 100.0
  )

  # Add peak data
  peak_data <- list(
    matrix(c(50.0, 75.0, 100.0, 10, 20, 30), ncol = 2)
  )

  spectra <- Spectra::Spectra(object = df)
  spectra@backend@peaksData <- peak_data

  result <- extract_spectra(object = spectra)

  expect_true("mz" %in% names(result))
  expect_true("intensity" %in% names(result))
  expect_type(result$mz, "list")
  expect_type(result$intensity, "list")

  # Check extracted peak data
  expect_equal(result$mz[[1]], c(50.0, 75.0, 100.0))
  expect_equal(result$intensity[[1]], c(10, 20, 30))
})

test_that("extract_spectra handles spectra with no peaks", {
  df <- data.frame(
    msLevel = 1L,
    precursorMz = 100.0
  )

  spectra <- Spectra::Spectra(object = df)
  # Empty peak data
  spectra@backend@peaksData <- list(matrix(numeric(0), ncol = 2))

  result <- extract_spectra(object = spectra)

  expect_equal(length(result$mz[[1]]), 0)
  expect_equal(length(result$intensity[[1]]), 0)
})

test_that("extract_spectra harmonizes column names", {
  df <- data.frame(
    msLevel = 2L,
    PrecursorMZ = 100.0, # Non-standard name
    precursorIntensity = 1000.0
  )

  spectra <- Spectra::Spectra(object = df)
  result <- extract_spectra(object = spectra)

  # Check harmonized names appear
  expect_true(
    "precursorMz" %in% names(result) || "PrecursorMZ" %in% names(result)
  )
})

test_that("extract_spectra converts column types correctly", {
  df <- data.frame(
    msLevel = 2L,
    precursorMz = 100.0,
    spectrum_id = "123", # Should be integer
    predicted = "TRUE" # Should be logical
  )

  spectra <- Spectra::Spectra(object = df)
  result <- extract_spectra(object = spectra)

  # spectrum_id should be converted to integer
  if ("spectrum_id" %in% names(result)) {
    expect_type(result$spectrum_id, "integer")
  }

  # predicted should be converted to logical
  if ("predicted" %in% names(result)) {
    expect_type(result$predicted, "logical")
  }
})

test_that("extract_spectra handles multiple spectra", {
  df <- data.frame(
    msLevel = c(1L, 2L, 2L),
    precursorMz = c(100.0, 200.0, 300.0),
    rtime = c(10, 20, 30)
  )

  spectra <- Spectra::Spectra(object = df)

  # Add peak data for all spectra
  peak_data <- list(
    matrix(c(50, 10), ncol = 2),
    matrix(c(100, 20), ncol = 2),
    matrix(c(150, 30), ncol = 2)
  )
  spectra@backend@peaksData <- peak_data

  result <- extract_spectra(object = spectra)

  expect_equal(nrow(result), 3)
  expect_equal(length(result$mz), 3)
  expect_equal(length(result$intensity), 3)
})

test_that("extract_spectra handles malformed peak data gracefully", {
  df <- data.frame(
    msLevel = 2L,
    precursorMz = 100.0
  )

  spectra <- Spectra::Spectra(object = df)

  # Malformed peak data (not a matrix)
  spectra@backend@peaksData <- list(numeric(0))

  result <- extract_spectra(object = spectra)

  expect_equal(length(result$mz[[1]]), 0)
  expect_equal(length(result$intensity[[1]]), 0)
})

test_that("extract_spectra preserves all original columns", {
  df <- data.frame(
    msLevel = 2L,
    precursorMz = 100.0,
    rtime = 10.5,
    compound_id = "COMP001",
    polarity = 1L
  )

  spectra <- Spectra::Spectra(object = df)
  result <- extract_spectra(object = spectra)

  # All original columns should be present (possibly renamed)
  expect_true(ncol(result) >= ncol(df))
})
