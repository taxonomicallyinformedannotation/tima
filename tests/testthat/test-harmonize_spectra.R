# ==============================================================================
# Test Suite: harmonize_spectra - Spectral Library Harmonization
# ==============================================================================
#
# @description
# Comprehensive unit tests for the harmonize_spectra function, which standardizes
# spectral library data from various sources into a common format.
#
# @coverage
# - Input validation (data frame, mode, column parameters)
# - Polarity filtering
# - Column name standardization
# - Missing column handling
# - Edge cases and error handling

# ==============================================================================
# Input Validation Tests
# ==============================================================================

test_that("harmonize_spectra validates input is a data frame", {
  expect_error(
    harmonize_spectra(
      spectra = "not a dataframe",
      metad = "test",
      mode = "pos",
      col_ad = "adduct",
      col_ce = "collision_energy",
      col_ci = "compound_id",
      col_em = "exact_mass",
      col_in = "inchi",
      col_io = "inchi_no_stereo",
      col_ik = "inchikey",
      col_il = "inchikey_layer",
      col_mf = "formula",
      col_na = "name",
      col_po = "polarity",
      col_sm = "smiles",
      col_sn = "smiles_no_stereo",
      col_si = "spectrum_id",
      col_sp = "splash",
      col_sy = "synonyms",
      col_xl = "xlogp"
    ),
    "must be a data frame"
  )
})

test_that("harmonize_spectra validates mode parameter", {
  test_spectra <- tidytable::tidytable(
    id = c(1, 2, 3)
  )

  # Mode must contain 'pos' or 'neg'
  expect_error(
    harmonize_spectra(
      spectra = test_spectra,
      metad = "test",
      mode = "invalid", # Invalid mode
      col_ad = "adduct",
      col_ce = "collision_energy",
      col_ci = "compound_id",
      col_em = "exact_mass",
      col_in = "inchi",
      col_io = "inchi_no_stereo",
      col_ik = "inchikey",
      col_il = "inchikey_layer",
      col_mf = "formula",
      col_na = "name",
      col_po = "polarity",
      col_sm = "smiles",
      col_sn = "smiles_no_stereo",
      col_si = "spectrum_id",
      col_sp = "splash",
      col_sy = "synonyms",
      col_xl = "xlogp"
    ),
    "must contain 'pos' or 'neg'"
  )
})

test_that("harmonize_spectra validates mode with regex pattern", {
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

test_that("harmonize_spectra mode validation is case insensitive", {
  test_spectra <- tidytable::tidytable(
    id = c(1),
    polarity = c("POSITIVE"),
    precursorMz = c(195.088),
    mz = I(list(c(100, 150, 195))),
    intensity = I(list(c(100, 500, 1000)))
  )

  # Should accept uppercase
  expect_no_error(
    harmonize_spectra(
      spectra = test_spectra,
      metad = "test",
      mode = "POSITIVE",
      col_ad = NULL,
      col_ce = NULL,
      col_ci = NULL,
      col_em = NULL,
      col_in = NULL,
      col_io = NULL,
      col_ik = NULL,
      col_il = NULL,
      col_mf = NULL,
      col_na = NULL,
      col_po = "polarity",
      col_sm = NULL,
      col_sn = NULL,
      col_si = NULL,
      col_sp = NULL,
      col_sy = NULL,
      col_xl = NULL
    )
  )

  # Should accept mixed case
  expect_no_error(
    harmonize_spectra(
      spectra = test_spectra,
      metad = "test",
      mode = "PoSiTiVe",
      col_ad = NULL,
      col_ce = NULL,
      col_ci = NULL,
      col_em = NULL,
      col_in = NULL,
      col_io = NULL,
      col_ik = NULL,
      col_il = NULL,
      col_mf = NULL,
      col_na = NULL,
      col_po = "polarity",
      col_sm = NULL,
      col_sn = NULL,
      col_si = NULL,
      col_sp = NULL,
      col_sy = NULL,
      col_xl = NULL
    )
  )
})

test_that("harmonize_spectra validates column name parameters are strings or NULL", {
  test_spectra <- tidytable::tidytable(
    id = c(1, 2, 3)
  )

  # Test non-character column parameter
  expect_error(
    harmonize_spectra(
      spectra = test_spectra,
      metad = "test",
      mode = "pos",
      col_ad = 123, # Should be character or NULL
      col_ce = "collision_energy",
      col_ci = "compound_id",
      col_em = "exact_mass",
      col_in = "inchi",
      col_io = "inchi_no_stereo",
      col_ik = "inchikey",
      col_il = "inchikey_layer",
      col_mf = "formula",
      col_na = "name",
      col_po = "polarity",
      col_sm = "smiles",
      col_sn = "smiles_no_stereo",
      col_si = "spectrum_id",
      col_sp = "splash",
      col_sy = "synonyms",
      col_xl = "xlogp"
    ),
    "must be single character strings or NULL"
  )

  # Test vector of column names (should be single string)
  expect_error(
    harmonize_spectra(
      spectra = test_spectra,
      metad = "test",
      mode = "pos",
      col_ad = c("adduct1", "adduct2"), # Should be single string
      col_ce = "collision_energy",
      col_ci = "compound_id",
      col_em = "exact_mass",
      col_in = "inchi",
      col_io = "inchi_no_stereo",
      col_ik = "inchikey",
      col_il = "inchikey_layer",
      col_mf = "formula",
      col_na = "name",
      col_po = "polarity",
      col_sm = "smiles",
      col_sn = "smiles_no_stereo",
      col_si = "spectrum_id",
      col_sp = "splash",
      col_sy = "synonyms",
      col_xl = "xlogp"
    ),
    "must be single character strings or NULL"
  )
})

test_that("harmonize_spectra validates vectorized column parameters", {
  test_spectra <- tidytable::tidytable(
    id = c(1, 2, 3)
  )

  # Multiple invalid parameters should be caught together
  expect_error(
    harmonize_spectra(
      spectra = test_spectra,
      metad = "test",
      mode = "pos",
      col_ad = 123, # Invalid
      col_ce = c("ce1", "ce2"), # Invalid (vector)
      col_ci = TRUE, # Invalid (not character)
      col_em = "exact_mass",
      col_in = "inchi",
      col_io = "inchi_no_stereo",
      col_ik = "inchikey",
      col_il = "inchikey_layer",
      col_mf = "formula",
      col_na = "name",
      col_po = "polarity",
      col_sm = "smiles",
      col_sn = "smiles_no_stereo",
      col_si = "spectrum_id",
      col_sp = "splash",
      col_sy = "synonyms",
      col_xl = "xlogp"
    ),
    "must be single character strings or NULL"
  )
})

# ==============================================================================
# Functional Tests
# ==============================================================================

# test_that("harmonize_spectra filters by polarity", {
#   df <- data.frame(
#     mode_col = c("POS", "NEG"),
#     precursorMz = c(100, 101),
#     mz = c(50, 51),
#     intensity = c(10, 11)
#   )
#   res <- harmonize_spectra(df, metad = "lib", mode = "pos", col_po = "mode_col")
#   expect_true(nrow(res) == 1)
# })

# test_that("harmonize_spectra adds missing columns", {
#   df <- data.frame(
#     mode_col = c("POS"),
#     precursorMz = 100,
#     mz = 50,
#     intensity = 10
#   )
#   res <- tima:::harmonize_spectra(df, metad = "lib", mode = "pos", col_ad = "", col_po = "mode_col")
#   expected_cols <- c(
#     "adduct",
#     "collision_energy",
#     "compound_id",
#     "exactmass",
#     "formula",
#     "inchi",
#     "inchi_no_stereo",
#     "inchikey",
#     "inchikey_connectivity_layer",
#     "name",
#     "smiles",
#     "smiles_no_stereo",
#     "spectrum_id",
#     "splash",
#     "synonyms",
#     "xlogp"
#   )
#   # Check that key columns are present
#   expect_true(all(c("precursorMz", "mz", "intensity") %in% names(res)))
# })
