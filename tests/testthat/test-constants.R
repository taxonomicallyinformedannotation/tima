# Test Suite: constants ----

library(testthat)

## Physical constants ----

test_that("test-constants ELECTRON_MASS_DALTONS is correct", {
  # CODATA 2018 recommended value
  expect_equal(ELECTRON_MASS_DALTONS, 5.485799E-4)
  expect_type(ELECTRON_MASS_DALTONS, "double")
  expect_true(ELECTRON_MASS_DALTONS > 0)
})

## Validation thresholds ----

test_that("test-constants MAX_TOLERANCE_PPM prevents excessive tolerances", {
  expect_equal(MAX_TOLERANCE_PPM, 20)
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

## Constants consistency ----

test_that("test-constants mass values are ordered correctly", {
  expect_true(MIN_MASS_DALTONS < MAX_MASS_DALTONS)
})

## Exported constants availability ----

test_that("test-constants all major constants are accessible", {
  # Physical constants
  expect_true(exists("ELECTRON_MASS_DALTONS"))

  # Tolerances
  expect_true(exists("MAX_TOLERANCE_PPM"))

  # Patterns
  expect_true(exists("ADDUCT_REGEX_PATTERN"))
  expect_true(exists("MODIFICATION_REGEX_PATTERN"))
})

test_that("get_constant returns existing constant", {
  val <- get_constant("DEFAULT_HC_SCORE_BIO_MIN")
  expect_equal(val, 0.85)
})

test_that("get_constant falls back to default when missing", {
  expect_warning(
    missing_val <- get_constant("NON_EXISTING_CONSTANT", default = 123),
    NA
  )
  expect_equal(missing_val, 123)
})

test_that("get_constant errors if missing and no default", {
  expect_error(get_constant("NON_EXISTING_CONSTANT"), "not found")
})

test_that("validate_against_constant succeeds for valid ms mode", {
  expect_true(validate_against_constant("pos", "VALID_MS_MODES"))
})

test_that("validate_against_constant errors for invalid ms mode", {
  expect_error(
    validate_against_constant("invalid", "VALID_MS_MODES"),
    "Invalid value"
  )
})
