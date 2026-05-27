# Test Suite: get_spectra_ids ----

library(testthat)

test_that("get_spectra_ids extracts IDs from SLAW_ID field", {
  # Create mock spectra with SLAW_ID
  sp_data <- data.frame(
    SLAW_ID = c("SLAW001", "SLAW002", "SLAW003"),
    precursorMz = c(100, 200, 300),
    msLevel = rep(2L, 3)
  )

  spectra <- Spectra::Spectra(object = sp_data)

  ids <- get_spectra_ids(spectra)

  expect_equal(ids, c("SLAW001", "SLAW002", "SLAW003"))
})

test_that("get_spectra_ids extracts IDs from FEATURE_ID field", {
  # Create mock spectra with FEATURE_ID (no SLAW_ID)
  sp_data <- data.frame(
    FEATURE_ID = c("FT001", "FT002", "FT003"),
    precursorMz = c(100, 200, 300),
    msLevel = rep(2L, 3)
  )

  spectra <- Spectra::Spectra(object = sp_data)

  ids <- get_spectra_ids(spectra)

  expect_equal(ids, c("FT001", "FT002", "FT003"))
})

test_that("get_spectra_ids extracts IDs from acquisitionNum field", {
  # Create mock spectra with acquisitionNum
  sp_data <- data.frame(
    acquisitionNum = c(1L, 2L, 3L),
    precursorMz = c(100, 200, 300),
    msLevel = rep(2L, 3)
  )

  spectra <- Spectra::Spectra(object = sp_data)

  ids <- get_spectra_ids(spectra)

  expect_equal(ids, c(1L, 2L, 3L))
})

test_that("get_spectra_ids extracts IDs from spectrum_id field", {
  # Create mock spectra with spectrum_id
  sp_data <- data.frame(
    spectrum_id = c("SP001", "SP002", "SP003"),
    precursorMz = c(100, 200, 300),
    msLevel = rep(2L, 3)
  )

  spectra <- Spectra::Spectra(object = sp_data)

  ids <- get_spectra_ids(spectra)

  expect_equal(ids, c("SP001", "SP002", "SP003"))
})

test_that("get_spectra_ids prioritizes SLAW_ID over other fields", {
  # Create spectra with multiple ID fields
  sp_data <- data.frame(
    SLAW_ID = c("SLAW001", "SLAW002"),
    FEATURE_ID = c("FT001", "FT002"),
    spectrum_id = c("SP001", "SP002"),
    precursorMz = c(100, 200),
    msLevel = rep(2L, 2)
  )

  spectra <- Spectra::Spectra(object = sp_data)

  ids <- get_spectra_ids(spectra)

  # Should return SLAW_ID (highest priority)
  expect_equal(ids, c("SLAW001", "SLAW002"))
})

test_that("get_spectra_ids returns NULL when no ID field found", {
  # Create spectra without any recognized ID fields or TITLE
  sp_data <- data.frame(
    precursorMz = c(100, 200, 300),
    msLevel = rep(2L, 3)
  )

  spectra <- Spectra::Spectra(object = sp_data)

  ids <- get_spectra_ids(spectra)

  expect_null(ids)
})

test_that("get_spectra_ids TITLE fallback: parses 'id:<N>' from masster-style TITLE", {
  # Newer masster variants embed the feature ID in TITLE as "id:42, rt:..., ..."
  sp_data <- data.frame(
    TITLE = c(
      "id:42, rt:168.84, mz:293.1762, energy:nan, sample_id:18, scan_id:2513, run_A",
      "id:43, rt:244.50, mz:347.2219, energy:nan, sample_id:18, scan_id:3080, run_A"
    ),
    precursorMz = c(293.1762, 347.2219),
    msLevel = rep(2L, 2)
  )

  spectra <- Spectra::Spectra(object = sp_data)

  # Should extract "42" and "43" from the TITLE field
  ids <- get_spectra_ids(spectra)
  expect_false(is.null(ids))
  expect_equal(as.character(ids), c("42", "43"))
})

test_that("get_spectra_ids TITLE fallback: does NOT match 'uid:', 'scan_id:', '_id:'", {
  # Old masster uses uid: (0-indexed), NOT id: as feature ID
  sp_data <- data.frame(
    TITLE = c(
      "uid:0, rt:96.71, mz:874.7831, MS1",
      "uid:1, rt:92.86, mz:846.7518, MS1"
    ),
    precursorMz = c(874.7831, 846.7518),
    msLevel = rep(2L, 2)
  )

  spectra <- Spectra::Spectra(object = sp_data)

  # uid: should NOT be matched — result should be NULL (no valid numeric ids)
  ids <- get_spectra_ids(spectra)
  expect_null(ids)
})

test_that("get_spectra_ids TITLE fallback: mixed TITLE — some with id:, some without", {
  # When only a subset of spectra have parseable id:, the result is still returned
  sp_data <- data.frame(
    TITLE = c(
      "id:10, rt:1.0, mz:100.0",
      "id:20, rt:2.0, mz:200.0"
    ),
    precursorMz = c(100.0, 200.0),
    msLevel = rep(2L, 2)
  )

  spectra <- Spectra::Spectra(object = sp_data)
  ids <- get_spectra_ids(spectra)
  expect_equal(as.character(ids), c("10", "20"))
})

test_that("get_spectra_ids validates input type", {
  # Non-Spectra input
  expect_error(
    get_spectra_ids(data.frame(x = 1)),
    "Spectra object",
    class = "tima_validation_error"
  )

  expect_error(
    get_spectra_ids(list(a = 1)),
    "Spectra object",
    class = "tima_validation_error"
  )

  expect_error(
    get_spectra_ids(NULL),
    "Spectra object",
    class = "tima_validation_error"
  )
})
