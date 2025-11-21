# Test Suite: export_spectra_rds ----

library(testthat)

test_that("export_spectra_rds validates file parameter", {
  # Missing file
  expect_error(
    export_spectra_rds(spectra = Spectra::Spectra()),
    "file must be a single character string"
  )

  # NULL file
  expect_error(
    export_spectra_rds(file = NULL, spectra = Spectra::Spectra()),
    "file must be a single character string"
  )

  # Non-character file
  expect_error(
    export_spectra_rds(file = 123, spectra = Spectra::Spectra()),
    "file must be a single character string"
  )

  # Multiple file paths
  expect_error(
    export_spectra_rds(
      file = c("path1.rds", "path2.rds"),
      spectra = Spectra::Spectra()
    ),
    "file must be a single character string"
  )
})

test_that("export_spectra_rds validates spectra parameter", {
  temp_file <- tempfile(fileext = ".rds")

  # Missing spectra
  expect_error(
    export_spectra_rds(file = temp_file),
    "spectra must be a Spectra object"
  )

  # Non-Spectra object
  expect_error(
    export_spectra_rds(file = temp_file, spectra = data.frame(a = 1)),
    "spectra must be a Spectra object"
  )

  expect_error(
    export_spectra_rds(file = temp_file, spectra = list()),
    "spectra must be a Spectra object"
  )
})

# test_that("export_spectra_rds handles empty spectra with no valid compound_id", {
#   temp_file <- tempfile(fileext = ".rds")
#
#   # Create Spectra without compound_id
#   spectra <- Spectra::Spectra()
#
#   expect_silent(result <- export_spectra_rds(file = temp_file, spectra = spectra))
#   expect_null(result)
#
#   # File should not be created if no valid spectra
#   expect_false(file.exists(temp_file))
# })

# test_that("export_spectra_rds handles spectra with NA compound_id", {
#   temp_file <- tempfile(fileext = ".rds")
#
#   # Create Spectra with NA compound_id
#   df <- data.frame(
#     msLevel = 2L,
#     precursorMz = 100.0,
#     compound_id = NA_character_
#   )
#   spectra <- Spectra::Spectra(df)
#
#   expect_silent(result <- export_spectra_rds(file = temp_file, spectra = spectra))
#   expect_null(result)
#
#   # File should not be created if no valid spectra
#   expect_false(file.exists(temp_file))
# })

test_that("export_spectra_rds exports spectra with valid compound_id", {
  temp_file <- tempfile(fileext = ".rds")

  # Create Spectra with valid compound_id
  df <- data.frame(
    msLevel = 2L,
    precursorMz = c(100.0, 200.0),
    compound_id = c("COMP001", "COMP002")
  )
  spectra <- Spectra::Spectra(df)

  expect_silent(
    result <- export_spectra_rds(file = temp_file, spectra = spectra)
  )
  expect_null(result)
  expect_true(file.exists(temp_file))

  # Verify file content
  loaded <- readRDS(temp_file)
  expect_s4_class(loaded, "Spectra")
  expect_equal(length(loaded), 2)
  expect_equal(loaded$compound_id, c("COMP001", "COMP002"))
})

test_that("export_spectra_rds filters out NA compound_id entries", {
  temp_file <- tempfile(fileext = ".rds")

  # Mix of valid and NA compound_id
  df <- data.frame(
    msLevel = 2L,
    precursorMz = c(100.0, 200.0, 300.0),
    compound_id = c("COMP001", NA_character_, "COMP003")
  )
  spectra <- Spectra::Spectra(df)

  expect_silent(export_spectra_rds(file = temp_file, spectra = spectra))
  expect_true(file.exists(temp_file))

  # Verify only valid compound_id spectra were exported
  loaded <- readRDS(temp_file)
  expect_equal(length(loaded), 2)
  expect_equal(loaded$compound_id, c("COMP001", "COMP003"))
})

test_that("export_spectra_rds creates output directory if needed", {
  temp_dir <- file.path(tempfile(), "nested", "dir", "structure")
  temp_file <- file.path(temp_dir, "spectra.rds")

  expect_false(dir.exists(temp_dir))

  df <- data.frame(
    msLevel = 2L,
    precursorMz = 100.0,
    compound_id = "COMP001"
  )
  spectra <- Spectra::Spectra(df)

  expect_silent(export_spectra_rds(file = temp_file, spectra = spectra))
  expect_true(file.exists(temp_file))
  expect_true(dir.exists(temp_dir))
})

test_that("export_spectra_rds overwrites existing file", {
  temp_file <- tempfile(fileext = ".rds")

  # Create first spectra
  df1 <- data.frame(
    msLevel = 2L,
    precursorMz = 100.0,
    compound_id = "COMP001"
  )
  spectra1 <- Spectra::Spectra(df1)
  export_spectra_rds(file = temp_file, spectra = spectra1)

  # Create second spectra with different data
  df2 <- data.frame(
    msLevel = 2L,
    precursorMz = c(200.0, 300.0),
    compound_id = c("COMP002", "COMP003")
  )
  spectra2 <- Spectra::Spectra(df2)
  export_spectra_rds(file = temp_file, spectra = spectra2)

  # Verify file was overwritten
  loaded <- readRDS(temp_file)
  expect_equal(length(loaded), 2)
  expect_equal(loaded$compound_id, c("COMP002", "COMP003"))
})
