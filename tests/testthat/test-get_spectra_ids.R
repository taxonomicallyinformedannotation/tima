# Test Suite: get_spectra_ids ----

library(testthat)

test_that("get_spectra_ids extracts IDs from SLAW_ID field", {
  skip_on_cran()

  # Create mock spectra with SLAW_ID
  sp_data <- data.frame(
    SLAW_ID = c("SLAW001", "SLAW002", "SLAW003"),
    precursorMz = c(100, 200, 300),
    msLevel = rep(2L, 3)
  )

  spectra <- Spectra::Spectra(sp_data)

  ids <- get_spectra_ids(spectra)

  expect_equal(ids, c("SLAW001", "SLAW002", "SLAW003"))
})

test_that("get_spectra_ids extracts IDs from FEATURE_ID field", {
  skip_on_cran()

  # Create mock spectra with FEATURE_ID (no SLAW_ID)
  sp_data <- data.frame(
    FEATURE_ID = c("FT001", "FT002", "FT003"),
    precursorMz = c(100, 200, 300),
    msLevel = rep(2L, 3)
  )

  spectra <- Spectra::Spectra(sp_data)

  ids <- get_spectra_ids(spectra)

  expect_equal(ids, c("FT001", "FT002", "FT003"))
})

test_that("get_spectra_ids extracts IDs from acquisitionNum field", {
  skip_on_cran()

  # Create mock spectra with acquisitionNum
  sp_data <- data.frame(
    acquisitionNum = c(1L, 2L, 3L),
    precursorMz = c(100, 200, 300),
    msLevel = rep(2L, 3)
  )

  spectra <- Spectra::Spectra(sp_data)

  ids <- get_spectra_ids(spectra)

  expect_equal(ids, c(1L, 2L, 3L))
})

test_that("get_spectra_ids extracts IDs from spectrum_id field", {
  skip_on_cran()

  # Create mock spectra with spectrum_id
  sp_data <- data.frame(
    spectrum_id = c("SP001", "SP002", "SP003"),
    precursorMz = c(100, 200, 300),
    msLevel = rep(2L, 3)
  )

  spectra <- Spectra::Spectra(sp_data)

  ids <- get_spectra_ids(spectra)

  expect_equal(ids, c("SP001", "SP002", "SP003"))
})

test_that("get_spectra_ids prioritizes SLAW_ID over other fields", {
  skip_on_cran()

  # Create spectra with multiple ID fields
  sp_data <- data.frame(
    SLAW_ID = c("SLAW001", "SLAW002"),
    FEATURE_ID = c("FT001", "FT002"),
    spectrum_id = c("SP001", "SP002"),
    precursorMz = c(100, 200),
    msLevel = rep(2L, 2)
  )

  spectra <- Spectra::Spectra(sp_data)

  ids <- get_spectra_ids(spectra)

  # Should return SLAW_ID (highest priority)
  expect_equal(ids, c("SLAW001", "SLAW002"))
})

test_that("get_spectra_ids returns NULL when no ID field found", {
  skip_on_cran()

  # Create spectra without any recognized ID fields
  sp_data <- data.frame(
    precursorMz = c(100, 200, 300),
    msLevel = rep(2L, 3)
  )

  spectra <- Spectra::Spectra(sp_data)

  ids <- get_spectra_ids(spectra)

  expect_null(ids)
})

test_that("get_spectra_ids validates input type", {
  skip_on_cran()

  # Non-Spectra input
  expect_error(
    get_spectra_ids(data.frame(x = 1)),
    "Input must be a Spectra object"
  )

  expect_error(
    get_spectra_ids(list(a = 1)),
    "Input must be a Spectra object"
  )

  expect_error(
    get_spectra_ids(NULL),
    "Input must be a Spectra object"
  )
})
