# Test Suite: constants ----

library(testthat)

## Physical constants ----

test_that("test-constants ELECTRON_MASS_DALTONS is correct", {
  # CODATA 2018 recommended value
  expect_equal(ELECTRON_MASS_DALTONS, 5.485799E-4)
  expect_type(ELECTRON_MASS_DALTONS, "double")
  expect_true(ELECTRON_MASS_DALTONS > 0)
})

test_that("test-constants PROTON_MASS_DALTONS is correct", {
  expect_equal(PROTON_MASS_DALTONS, 1.007276466812)
  expect_type(PROTON_MASS_DALTONS, "double")
  expect_true(PROTON_MASS_DALTONS > 1)
})

## Default tolerance values ----

test_that("test-constants tolerance values are reasonable", {
  # MS1 tolerance
  expect_equal(DEFAULT_TOLERANCE_PPM_MS1, 10)
  expect_true(DEFAULT_TOLERANCE_PPM_MS1 > 0)
  expect_true(DEFAULT_TOLERANCE_PPM_MS1 <= MAX_TOLERANCE_PPM)

  # MS2 tolerance
  expect_equal(DEFAULT_TOLERANCE_PPM_MS2, 10)
  expect_true(DEFAULT_TOLERANCE_PPM_MS2 > 0)

  # RT tolerances
  expect_equal(DEFAULT_TOLERANCE_RT_ADDUCTS, 0.02)
  expect_true(DEFAULT_TOLERANCE_RT_ADDUCTS > 0)
  expect_equal(DEFAULT_TOLERANCE_RT_LIBRARY, Inf)
})

## Validation thresholds ----

test_that("test-constants MAX_TOLERANCE_PPM prevents excessive tolerances", {
  expect_equal(MAX_TOLERANCE_PPM, 20)
  expect_true(MAX_TOLERANCE_PPM >= DEFAULT_TOLERANCE_PPM_MS1)
})

test_that("test-constants mass ranges are sensible", {
  expect_equal(MIN_MASS_DALTONS, 0)
  expect_equal(MAX_MASS_DALTONS, 5000)
  expect_true(MAX_MASS_DALTONS > MIN_MASS_DALTONS)
})

## Regular expression patterns ----

test_that("test-constants ADDUCT_REGEX_PATTERN matches valid adducts", {
  expect_type(ADDUCT_REGEX_PATTERN, "character")

  # Test with simple adduct
  expect_true(grepl(ADDUCT_REGEX_PATTERN, "[M+H]+", perl = TRUE))
  expect_true(grepl(ADDUCT_REGEX_PATTERN, "[M-H]-", perl = TRUE))

  # Test with complex adduct
  expect_true(grepl(ADDUCT_REGEX_PATTERN, "[2M+Na]+", perl = TRUE))
  expect_true(
    grepl(ADDUCT_REGEX_PATTERN, "[M-H2O+H]+", perl = TRUE)
  )

  # Should not match invalid patterns
  expect_false(grepl(ADDUCT_REGEX_PATTERN, "invalid", perl = TRUE))
  expect_false(grepl(ADDUCT_REGEX_PATTERN, "M+H", perl = TRUE))
})

test_that("test-constants MODIFICATION_REGEX_PATTERN is valid", {
  expect_type(MODIFICATION_REGEX_PATTERN, "character")

  # Test with modifications (using perl=TRUE for \w support)
  expect_true(grepl(MODIFICATION_REGEX_PATTERN, "+H", perl = TRUE))
  expect_true(grepl(MODIFICATION_REGEX_PATTERN, "-H2O", perl = TRUE))
  expect_true(grepl(MODIFICATION_REGEX_PATTERN, "+Na", perl = TRUE))
})

## File extensions ----

test_that("test-constants COMPRESSED_EXTENSIONS is complete", {
  expect_type(COMPRESSED_EXTENSIONS, "character")
  expect_true(".gz" %in% COMPRESSED_EXTENSIONS)
  expect_true(".zip" %in% COMPRESSED_EXTENSIONS)
  expect_true(".bz2" %in% COMPRESSED_EXTENSIONS)
  expect_true(length(COMPRESSED_EXTENSIONS) >= 3)
})

test_that("test-constants TABLE_EXTENSIONS is complete", {
  expect_type(TABLE_EXTENSIONS, "character")
  expect_true(".tsv" %in% TABLE_EXTENSIONS)
  expect_true(".csv" %in% TABLE_EXTENSIONS)
  expect_true(".txt" %in% TABLE_EXTENSIONS)
  expect_true(length(TABLE_EXTENSIONS) >= 3)
})

## Adduct masses ----

test_that("test-constants ADDUCT_MASSES contains common adducts", {
  expect_type(ADDUCT_MASSES, "list")

  # Check for essential adducts
  expect_true("H" %in% names(ADDUCT_MASSES))
  expect_true("Na" %in% names(ADDUCT_MASSES))
  expect_true("K" %in% names(ADDUCT_MASSES))
  expect_true("NH4" %in% names(ADDUCT_MASSES))

  # Verify masses are positive numbers
  expect_true(all(sapply(ADDUCT_MASSES, function(x) is.numeric(x) && x > 0)))
})

test_that("test-constants ADDUCT_MASSES values are accurate", {
  # Hydrogen
  expect_equal(ADDUCT_MASSES$H, 1.007825032)

  # Sodium (monoisotopic mass)
  expect_equal(ADDUCT_MASSES$Na, 22.98976928)

  # Potassium
  expect_equal(ADDUCT_MASSES$K, 38.96370668)

  # Ammonium
  expect_equal(ADDUCT_MASSES$NH4, 18.033823)
})

test_that("test-constants ADDUCT_MASSES includes organic acid adducts", {
  expect_true("HCOO" %in% names(ADDUCT_MASSES)) # Formate
  expect_true("CH3COO" %in% names(ADDUCT_MASSES)) # Acetate
  expect_true("CF3COO" %in% names(ADDUCT_MASSES)) # Trifluoroacetate

  # Verify formate mass
  expect_equal(ADDUCT_MASSES$HCOO, 44.9976542)
})

## Constants consistency ----

test_that("test-constants default tolerances respect maximum limits", {
  expect_true(DEFAULT_TOLERANCE_PPM_MS1 <= MAX_TOLERANCE_PPM)
  expect_true(DEFAULT_TOLERANCE_PPM_MS2 <= MAX_TOLERANCE_PPM)
  expect_true(DEFAULT_TOLERANCE_RT_ADDUCTS <= MAX_TOLERANCE_RT_ADDUCTS)
})

test_that("test-constants mass values are ordered correctly", {
  expect_true(MIN_MASS_DALTONS < MAX_MASS_DALTONS)
  expect_true(ELECTRON_MASS_DALTONS < PROTON_MASS_DALTONS)
})

## Exported constants availability ----

test_that("test-constants all major constants are accessible", {
  # Physical constants
  expect_true(exists("ELECTRON_MASS_DALTONS"))
  expect_true(exists("PROTON_MASS_DALTONS"))

  # Tolerances
  expect_true(exists("DEFAULT_TOLERANCE_PPM_MS1"))
  expect_true(exists("MAX_TOLERANCE_PPM"))

  # Patterns
  expect_true(exists("ADDUCT_REGEX_PATTERN"))
  expect_true(exists("MODIFICATION_REGEX_PATTERN"))

  # Extensions
  expect_true(exists("COMPRESSED_EXTENSIONS"))
  expect_true(exists("TABLE_EXTENSIONS"))

  # Masses
  expect_true(exists("ADDUCT_MASSES"))
})
