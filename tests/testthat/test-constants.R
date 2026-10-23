# Test Suite: constants ----

library(testthat)

## Physical constants ----

test_that("test-constants ELECTRON_MASS_DALTONS is correct", {
  # CODATA 2018 recommended value
  expect_equal(ELECTRON_MASS_DALTONS, 0.000548579909065)
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

  # Simple adducts
  expect_true(grepl(ADDUCT_REGEX_PATTERN, "[M+H]+", perl = TRUE))
  expect_true(grepl(ADDUCT_REGEX_PATTERN, "[M-H]-", perl = TRUE))

  # Multimers
  expect_true(grepl(ADDUCT_REGEX_PATTERN, "[2M+Na]+", perl = TRUE))
  expect_true(grepl(ADDUCT_REGEX_PATTERN, "[3M+H]+", perl = TRUE))

  # Modifications
  expect_true(
    grepl(ADDUCT_REGEX_PATTERN, "[M-H2O+H]+", perl = TRUE)
  )

  # Isotope shift notation (signed, e.g. [M+1+H]+)
  expect_true(grepl(ADDUCT_REGEX_PATTERN, "[M+1+H]+", perl = TRUE))
  expect_true(grepl(ADDUCT_REGEX_PATTERN, "[M-2+H]+", perl = TRUE))
  expect_true(grepl(ADDUCT_REGEX_PATTERN, "[2M+1+Na]+", perl = TRUE))

  # Charged adducts
  expect_true(grepl(ADDUCT_REGEX_PATTERN, "[M+2H]2+", perl = TRUE))

  # Should not match invalid patterns
  expect_false(grepl(ADDUCT_REGEX_PATTERN, "invalid", perl = TRUE))
  expect_false(grepl(ADDUCT_REGEX_PATTERN, "M+H", perl = TRUE))
  # Anchors prevent partial matches like trailing characters
  expect_false(grepl(ADDUCT_REGEX_PATTERN, "[M+H]+ garbage", perl = TRUE))
  expect_false(grepl(ADDUCT_REGEX_PATTERN, "garbage [M+H]+", perl = TRUE))

  # Isotope label notation: 13C should NOT be captured as isotope shift
  expect_true(grepl(ADDUCT_REGEX_PATTERN, "[M+13C+H]+", perl = TRUE))
  expect_true(grepl(ADDUCT_REGEX_PATTERN, "[M+15N+H]+", perl = TRUE))
})

test_that("test-constants MODIFICATION_REGEX_PATTERN matches individual modifications", {
  expect_type(MODIFICATION_REGEX_PATTERN, "character")

  # Simple additions
  expect_true(grepl(MODIFICATION_REGEX_PATTERN, "+H", perl = TRUE))
  expect_true(grepl(MODIFICATION_REGEX_PATTERN, "+Na", perl = TRUE))
  expect_true(grepl(MODIFICATION_REGEX_PATTERN, "+K", perl = TRUE))

  # Losses
  expect_true(grepl(MODIFICATION_REGEX_PATTERN, "-H2O", perl = TRUE))
  expect_true(grepl(MODIFICATION_REGEX_PATTERN, "-NH3", perl = TRUE))

  # Coefficients
  expect_true(grepl(MODIFICATION_REGEX_PATTERN, "+2Na", perl = TRUE))
  expect_true(grepl(MODIFICATION_REGEX_PATTERN, "-2H2O", perl = TRUE))

  # Complex formulas
  expect_true(grepl(MODIFICATION_REGEX_PATTERN, "+C6H12O6", perl = TRUE))
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
  val <- get_constant("DEFAULT_HE_SCORE_BIO_MIN")
  expect_equal(val, 0.8)
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
    "invalid value"
  )
})
