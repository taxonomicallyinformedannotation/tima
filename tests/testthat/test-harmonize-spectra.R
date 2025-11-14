# Test: Harmonize Spectra - Spectral Library Harmonization
library(testthat)

# =============================================================================
# Tests for harmonize_spectra() - Library Standardization
# =============================================================================

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

# test_that("harmonize_spectra accepts valid mode values", {
#   test_spectra <- tidytable::tidytable(
#     id = c(1),
#     adduct = c("[M+H]+"),
#     polarity = c("positive")
#   )
#
#   # Should accept "positive"
#   expect_no_error(
#     harmonize_spectra(
#       spectra = test_spectra,
#       metad = "test",
#       mode = "positive",
#       col_ad = "adduct",
#       col_ce = NULL,
#       col_ci = NULL,
#       col_em = NULL,
#       col_in = NULL,
#       col_io = NULL,
#       col_ik = NULL,
#       col_il = NULL,
#       col_mf = NULL,
#       col_na = NULL,
#       col_po = "polarity",
#       col_sm = NULL,
#       col_sn = NULL,
#       col_si = NULL,
#       col_sp = NULL,
#       col_sy = NULL,
#       col_xl = NULL
#     )
#   )
#
#   # Should accept "negative"
#   test_spectra_neg <- tidytable::tidytable(
#     id = c(1),
#     adduct = c("[M-H]-"),
#     polarity = c("negative")
#   )
#
#   expect_no_error(
#     harmonize_spectra(
#       spectra = test_spectra_neg,
#       metad = "test",
#       mode = "negative",
#       col_ad = "adduct",
#       col_ce = NULL,
#       col_ci = NULL,
#       col_em = NULL,
#       col_in = NULL,
#       col_io = NULL,
#       col_ik = NULL,
#       col_il = NULL,
#       col_mf = NULL,
#       col_na = NULL,
#       col_po = "polarity",
#       col_sm = NULL,
#       col_sn = NULL,
#       col_si = NULL,
#       col_sp = NULL,
#       col_sy = NULL,
#       col_xl = NULL
#     )
#   )
# })

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

# test_that("harmonize_spectra accepts NULL for optional column parameters", {
#   test_spectra <- tidytable::tidytable(
#     id = c(1),
#     name = c("Test Compound"),
#     polarity = c("positive")
#   )
#
#   # Should work with many NULL column parameters
#   expect_no_error({
#     result <- harmonize_spectra(
#       spectra = test_spectra,
#       metad = "test",
#       mode = "positive",
#       col_ad = NULL, # NULL is valid
#       col_ce = NULL,
#       col_ci = NULL,
#       col_em = NULL,
#       col_in = NULL,
#       col_io = NULL,
#       col_ik = NULL,
#       col_il = NULL,
#       col_mf = NULL,
#       col_na = "name",
#       col_po = "polarity",
#       col_sm = NULL,
#       col_sn = NULL,
#       col_si = NULL,
#       col_sp = NULL,
#       col_sy = NULL,
#       col_xl = NULL
#     )
#   })
# })

# test_that("harmonize_spectra handles empty data frame", {
#   empty_spectra <- tidytable::tidytable(
#     name = character(0),
#     polarity = character(0)
#   )
#
#   # Should handle empty input gracefully
#   expect_no_error({
#     result <- harmonize_spectra(
#       spectra = empty_spectra,
#       metad = "test",
#       mode = "positive",
#       col_ad = NULL,
#       col_ce = NULL,
#       col_ci = NULL,
#       col_em = NULL,
#       col_in = NULL,
#       col_io = NULL,
#       col_ik = NULL,
#       col_il = NULL,
#       col_mf = NULL,
#       col_na = "name",
#       col_po = "polarity",
#       col_sm = NULL,
#       col_sn = NULL,
#       col_si = NULL,
#       col_sp = NULL,
#       col_sy = NULL,
#       col_xl = NULL
#     )
#
#     expect_s3_class(result, "data.frame")
#     expect_equal(nrow(result), 0)
#   })
# })

# test_that("harmonize_spectra standardizes column names", {
#   test_spectra <- tidytable::tidytable(
#     my_compound_name = c("Caffeine", "Theobromine"),
#     my_adduct = c("[M+H]+", "[M+Na]+"),
#     my_polarity = c("positive", "positive"),
#     my_formula = c("C8H10N4O2", "C7H8N4O2"),
#     my_smiles = c("CN1C=NC2=C1C(=O)N(C(=O)N2C)C", "CN1C=NC2=C1C(=O)NC(=O)N2C")
#   )
#
#   result <- harmonize_spectra(
#     spectra = test_spectra,
#     metad = "custom_library",
#     mode = "positive",
#     col_ad = "my_adduct",
#     col_ce = NULL,
#     col_ci = NULL,
#     col_em = NULL,
#     col_in = NULL,
#     col_io = NULL,
#     col_ik = NULL,
#     col_il = NULL,
#     col_mf = "my_formula",
#     col_na = "my_compound_name",
#     col_po = "my_polarity",
#     col_sm = "my_smiles",
#     col_sn = NULL,
#     col_si = NULL,
#     col_sp = NULL,
#     col_sy = NULL,
#     col_xl = NULL
#   )
#
#   # Check that result has standardized column names
#   expect_s3_class(result, "data.frame")
#   expect_equal(nrow(result), 2)
# })

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

# test_that("harmonize_spectra mode validation is case insensitive", {
#   test_spectra <- tidytable::tidytable(
#     id = c(1),
#     polarity = c("POSITIVE")
#   )
#
#   # Should accept uppercase
#   expect_no_error(
#     harmonize_spectra(
#       spectra = test_spectra,
#       metad = "test",
#       mode = "POSITIVE",
#       col_ad = NULL,
#       col_ce = NULL,
#       col_ci = NULL,
#       col_em = NULL,
#       col_in = NULL,
#       col_io = NULL,
#       col_ik = NULL,
#       col_il = NULL,
#       col_mf = NULL,
#       col_na = NULL,
#       col_po = "polarity",
#       col_sm = NULL,
#       col_sn = NULL,
#       col_si = NULL,
#       col_sp = NULL,
#       col_sy = NULL,
#       col_xl = NULL
#     )
#   )
#
#   # Should accept mixed case
#   expect_no_error(
#     harmonize_spectra(
#       spectra = test_spectra,
#       metad = "test",
#       mode = "PoSiTiVe",
#       col_ad = NULL,
#       col_ce = NULL,
#       col_ci = NULL,
#       col_em = NULL,
#       col_in = NULL,
#       col_io = NULL,
#       col_ik = NULL,
#       col_il = NULL,
#       col_mf = NULL,
#       col_na = NULL,
#       col_po = "polarity",
#       col_sm = NULL,
#       col_sn = NULL,
#       col_si = NULL,
#       col_sp = NULL,
#       col_sy = NULL,
#       col_xl = NULL
#     )
#   )
# })
