# ==============================================================================
# Test Suite: harmonize_spectra
# ==============================================================================
library(testthat)
library(tima)

test_that("harmonize_spectra validates mode", {
  df <- data.frame(
    mode_col = c("POS", "NEG"),
    precursorMz = 100,
    mz = 50,
    intensity = 10
  )
  expect_error(
    harmonize_spectra(df, metad = "lib", mode = "foo", col_po = "mode_col"),
    "pos.*neg"
  )
})

test_that("harmonize_spectra filters by polarity", {
  df <- data.frame(
    mode_col = c("POS", "NEG"),
    precursorMz = c(100, 101),
    mz = c(50, 51),
    intensity = c(10, 11)
  )
  res <- harmonize_spectra(df, metad = "lib", mode = "pos", col_po = "mode_col")
  expect_true(nrow(res) == 1)
})

test_that("harmonize_spectra adds missing columns", {
  df <- data.frame(
    mode_col = c("POS"),
    precursorMz = 100,
    mz = 50,
    intensity = 10
  )
  res <- harmonize_spectra(df, metad = "lib", mode = "pos", col_po = "mode_col")
  expected_cols <- c(
    "adduct",
    "collision_energy",
    "compound_id",
    "exactmass",
    "formula",
    "inchi",
    "inchi_no_stereo",
    "inchikey",
    "inchikey_connectivity_layer",
    "name",
    "smiles",
    "smiles_no_stereo",
    "spectrum_id",
    "splash",
    "synonyms",
    "xlogp"
  )
  expect_true(all(expected_cols %in% names(res)))
})
