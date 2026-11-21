# Test Suite: harmonize_spectra ----

library(testthat)

call_harmonize_spectra <- function(
  spectra,
  metad = "test",
  mode = "pos",
  ...,
  .null_cols = FALSE
) {
  cols <- if (.null_cols) {
    list(
      col_ad = NULL,
      col_ce = NULL,
      col_ci = NULL,
      col_in = NULL,
      col_io = NULL,
      col_ik = NULL,
      col_il = NULL,
      col_na = NULL,
      col_po = "polarity",
      col_sm = NULL,
      col_sn = NULL,
      col_si = NULL,
      col_sp = NULL,
      col_sy = NULL
    )
  } else {
    list(
      col_ad = "adduct",
      col_ce = "collision_energy",
      col_ci = "compound_id",
      col_in = "inchi",
      col_io = "inchi_no_stereo",
      col_ik = "inchikey",
      col_il = "inchikey_layer",
      col_na = "name",
      col_po = "polarity",
      col_sm = "smiles",
      col_sn = "smiles_no_stereo",
      col_si = "spectrum_id",
      col_sp = "splash",
      col_sy = "synonyms"
    )
  }
  args <- c(list(spectra = spectra, metad = metad, mode = mode), cols)
  overrides <- list(...)
  if (length(overrides) > 0L) {
    args[names(overrides)] <- overrides
  }
  do.call(harmonize_spectra, args)
}

## Input Validation ----

test_that("harmonize_spectra validates input is a data frame", {
  expect_error(
    call_harmonize_spectra(spectra = "not a dataframe"),
    "Fix: Ensure input is a valid data frame",
    fixed = TRUE
  )
})

test_that("harmonize_spectra validates mode parameter", {
  test_spectra <- tidytable::tidytable(
    id = c(1, 2, 3)
  )

  # Mode must contain 'pos' or 'neg'
  expect_error(
    call_harmonize_spectra(spectra = test_spectra, mode = "invalid"),
    "must contain 'pos' or 'neg'",
    class = "tima_validation_error"
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
    "pos.*neg",
    class = "tima_validation_error"
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
    call_harmonize_spectra(
      spectra = test_spectra,
      mode = "POSITIVE",
      .null_cols = TRUE
    )
  )

  # Should accept mixed case
  expect_no_error(
    call_harmonize_spectra(
      spectra = test_spectra,
      mode = "PoSiTiVe",
      .null_cols = TRUE
    )
  )
})

test_that("harmonize_spectra validates column name parameters are strings or NULL", {
  test_spectra <- tidytable::tidytable(
    id = c(1, 2, 3)
  )

  # Test non-character column parameter
  expect_error(
    call_harmonize_spectra(spectra = test_spectra, col_ad = 123),
    "Column name parameter\\(s\\) must satisfy validation.*col_ad"
  )

  # Test vector of column names (should be single string)
  expect_error(
    call_harmonize_spectra(
      spectra = test_spectra,
      col_ad = c("adduct1", "adduct2")
    ),
    "Column name parameter\\(s\\) must satisfy validation.*col_ad"
  )
})

test_that("harmonize_spectra validates column parameters", {
  test_spectra <- tidytable::tidytable(
    id = c(1, 2, 3)
  )

  # Multiple invalid parameters should be caught together
  expect_error(
    call_harmonize_spectra(
      spectra = test_spectra,
      col_ad = 123,
      col_ce = c("ce1", "ce2"),
      col_ci = TRUE
    ),
    "Column name parameter\\(s\\) must satisfy validation.*col_ad, col_ce, col_ci"
  )
})

test_that("harmonize_spectra populates defaults and preserves list columns for matching polarities", {
  test_spectra <- tidytable::tidytable(
    polarity = c("Positive", "Negative", "1", "-1"),
    name_col = c("Caffeine", "NegCmpd", "Aspirin", "Neg2"),
    smiles_col = c(
      "CN1C=NC2=C1C(=O)N(C(=O)N2C)C",
      "CC(=O)O",
      "CC(=O)OC1=CC=CC=C1C(=O)O",
      "CCO"
    ),
    precursorMz = c(195.088, 60.02, 181.05, 45.03),
    mz = list(c(100, 195), c(40, 60), c(120, 181), c(30, 45)),
    intensity = list(c(50, 100), c(20, 80), c(40, 90), c(10, 50))
  )

  res <- call_harmonize_spectra(
    spectra = test_spectra,
    metad = "TestLib",
    col_na = "name_col",
    col_sm = "smiles_col",
    .null_cols = TRUE
  )

  expect_equal(nrow(res), 2L)
  expect_true(all(res$precursorCharge == 1L))
  expect_true(all(res$library == "TestLib"))
  expect_true(is.list(res$mz))
  expect_true(is.list(res$intensity))
  expect_setequal(res$compound_id, c("Caffeine", "Aspirin"))
  expect_setequal(res$spectrum_id, c("1", "2"))

  # Also works when all 13 column mappings are provided (columns_missing is empty)
  res_all_cols <- call_harmonize_spectra(
    spectra = test_spectra,
    metad = "TestLib",
    col_il = "inchikey_connectivity_layer",
    col_na = "name_col",
    col_sm = "smiles_col"
  )
  expect_equal(nrow(res_all_cols), 2L)
  expect_setequal(res_all_cols$compound_id, c("Caffeine", "Aspirin"))

  # Partial NA precursorCharge, rtime column, partial NA spectrum_id and compound_id
  spec_partial <- tidytable::tidytable(
    polarity = c("Positive", "Positive"),
    name_col = c("Caffeine", "Aspirin"),
    smiles_col = c("CN1C=NC2=C1C(=O)N(C(=O)N2C)C", "CC(=O)OC1=CC=CC=C1C(=O)O"),
    precursorMz = c(195.088, 181.05),
    precursorCharge = c(1L, NA_integer_),
    rtime = c(12.5, 25.0),
    spectrum_id = c("SPEC_A", NA_character_),
    compound_id = c("CMPD_A", NA_character_),
    mz = list(c(100, 195), c(120, 181)),
    intensity = list(c(50, 100), c(40, 90))
  )
  res_partial <- call_harmonize_spectra(
    spectra = spec_partial,
    metad = "TestLib",
    col_il = "inchikey_connectivity_layer",
    col_na = "name_col",
    col_sm = "smiles_col"
  )
  expect_equal(res_partial$precursorCharge, c(1L, 1L))
  expect_equal(res_partial$rtime, c(12.5, 25.0))
  expect_setequal(res_partial$spectrum_id, c("SPEC_A", "2"))
  expect_setequal(res_partial$compound_id, c("CMPD_A", "Aspirin"))
})
