# Test Suite: harmonize_names_sirius ----
# Tests for SIRIUS column name harmonization

## Input Validation Tests ----

test_that("harmonize_names_sirius requires input", {
  expect_error(
    harmonize_names_sirius(),
    "must be provided"
  )
})

test_that("harmonize_names_sirius handles NULL", {
  expect_error(
    harmonize_names_sirius(NULL),
    "must be provided"
  )
})

test_that("harmonize_names_sirius handles non-character input", {
  # Should convert to character
  expect_type(harmonize_names_sirius(123), "character")
  expect_type(harmonize_names_sirius(TRUE), "character")
})

## Basic Functionality Tests ----

test_that("harmonize_names_sirius removes numeric prefixes", {
  expect_equal(harmonize_names_sirius("1_compound_name"), "name")
  expect_equal(harmonize_names_sirius("2_feature_id"), "id")
  expect_equal(harmonize_names_sirius("10_spectrum_score"), "score")
})

test_that("harmonize_names_sirius handles multiple underscores", {
  # Should remove everything up to and including LAST underscore
  expect_equal(harmonize_names_sirius("1_compound_name"), "name")
  expect_equal(harmonize_names_sirius("1_2_compound_name"), "name")
  expect_equal(harmonize_names_sirius("a_b_c_name"), "name")
})

test_that("harmonize_names_sirius handles no underscores", {
  # Should return original if no underscore
  expect_equal(harmonize_names_sirius("score"), "score")
  expect_equal(harmonize_names_sirius("name"), "name")
  expect_equal(harmonize_names_sirius("id"), "id")
})

test_that("harmonize_names_sirius handles single underscore", {
  expect_equal(harmonize_names_sirius("compound_name"), "name")
  expect_equal(harmonize_names_sirius("feature_id"), "id")
  expect_equal(harmonize_names_sirius("_value"), "value")
})

test_that("harmonize_names_sirius handles trailing underscore", {
  expect_equal(harmonize_names_sirius("name_"), "")
  expect_equal(harmonize_names_sirius("compound_"), "")
})

test_that("harmonize_names_sirius handles leading underscore", {
  expect_equal(harmonize_names_sirius("_name"), "name")
  expect_equal(harmonize_names_sirius("_compound_name"), "name")
})

## SIRIUS-Specific Pattern Tests ----

test_that("harmonize_names_sirius handles typical SIRIUS patterns", {
  # Common SIRIUS column name patterns
  expect_equal(harmonize_names_sirius("1_molecularFormula"), "molecularFormula")
  expect_equal(harmonize_names_sirius("2_adduct"), "adduct")
  expect_equal(harmonize_names_sirius("3_precursorFormula"), "precursorFormula")
  expect_equal(harmonize_names_sirius("4_SiriusScore"), "SiriusScore")
  expect_equal(harmonize_names_sirius("5_TreeScore"), "TreeScore")
  expect_equal(harmonize_names_sirius("6_IsotopeScore"), "IsotopeScore")
})

test_that("harmonize_names_sirius preserves case", {
  expect_equal(harmonize_names_sirius("1_CamelCase"), "CamelCase")
  expect_equal(harmonize_names_sirius("1_UPPERCASE"), "UPPERCASE")
  expect_equal(harmonize_names_sirius("1_lowercase"), "lowercase")
})

## Edge Cases ----

test_that("harmonize_names_sirius handles empty string", {
  expect_equal(harmonize_names_sirius(""), "")
})

test_that("harmonize_names_sirius handles only underscores", {
  expect_equal(harmonize_names_sirius("_"), "")
  expect_equal(harmonize_names_sirius("__"), "")
  expect_equal(harmonize_names_sirius("___"), "")
})

test_that("harmonize_names_sirius handles special characters", {
  expect_equal(harmonize_names_sirius("1_name@test"), "name@test")
  expect_equal(harmonize_names_sirius("1_name.value"), "name.value")
  expect_equal(harmonize_names_sirius("1_name-id"), "name-id")
})

test_that("harmonize_names_sirius handles numbers in final part", {
  expect_equal(harmonize_names_sirius("1_score123"), "score123")
  expect_equal(harmonize_names_sirius("2_level2"), "level2")
  expect_equal(harmonize_names_sirius("3_m1"), "m1")
})

test_that("harmonize_names_sirius handles very long names", {
  long_name <- paste0("1_", paste(rep("name", 100), collapse = "_"))
  result <- harmonize_names_sirius(long_name)
  expect_equal(result, "name")
})

## Vectorization Tests ----

test_that("harmonize_names_sirius works element-wise on vectors", {
  input <- c("1_name", "2_id", "3_score")
  result <- sapply(input, harmonize_names_sirius, USE.NAMES = FALSE)

  expect_equal(result, c("name", "id", "score"))
  expect_length(result, 3)
})

test_that("harmonize_names_sirius handles mixed patterns in vector", {
  input <- c("1_name", "noprefix", "2_3_id", "_value")
  result <- sapply(input, harmonize_names_sirius, USE.NAMES = FALSE)

  expect_equal(result, c("name", "noprefix", "id", "value"))
})

## Return Value Tests ----

test_that("harmonize_names_sirius always returns character", {
  expect_type(harmonize_names_sirius("1_name"), "character")
  expect_type(harmonize_names_sirius("name"), "character")
  expect_type(harmonize_names_sirius(""), "character")
})

test_that("harmonize_names_sirius returns single value", {
  expect_length(harmonize_names_sirius("1_name"), 1)
  expect_length(harmonize_names_sirius("test"), 1)
})

## Integration Tests ----

test_that("harmonize_names_sirius works with real SIRIUS output", {
  # Simulate actual SIRIUS column names
  sirius_cols <- c(
    "1_id",
    "2_molecularFormula",
    "3_adduct",
    "4_precursorFormula",
    "5_SiriusScore",
    "6_TreeScore",
    "7_IsotopeScore",
    "8_numExplainedPeaks",
    "9_explainedIntensity",
    "10_medianMassErrorFragmentPeaks(ppm)",
    "11_medianAbsoluteMassErrorFragmentPeaks(ppm)"
  )

  result <- sapply(sirius_cols, harmonize_names_sirius, USE.NAMES = FALSE)

  expect_equal(result[1], "id")
  expect_equal(result[2], "molecularFormula")
  expect_equal(result[3], "adduct")
  expect_equal(result[5], "SiriusScore")
  # Last one has parentheses
  expect_true(grepl("ppm\\)", result[11]))
})

test_that("harmonize_names_sirius can be used with dplyr rename", {
  # Simulate renaming columns
  old_names <- c("1_name", "2_id", "3_score")
  new_names <- sapply(old_names, harmonize_names_sirius, USE.NAMES = FALSE)

  expect_equal(new_names, c("name", "id", "score"))
  expect_false(any(grepl("^[0-9]+_", new_names)))
})

## Consistency Tests ----

test_that("harmonize_names_sirius is deterministic", {
  input <- "1_compound_name"

  result1 <- harmonize_names_sirius(input)
  result2 <- harmonize_names_sirius(input)
  result3 <- harmonize_names_sirius(input)

  expect_identical(result1, result2)
  expect_identical(result2, result3)
})

test_that("harmonize_names_sirius handles repeated calls", {
  input <- "1_name"

  # First call
  result1 <- harmonize_names_sirius(input)
  # Second call on result (idempotent for already harmonized names)
  result2 <- harmonize_names_sirius(result1)

  expect_equal(result1, "name")
  expect_equal(result2, "name")
})

## Performance Tests ----

test_that("harmonize_names_sirius is reasonably fast", {
  skip_if_not_installed("bench")

  timing <- bench::mark(
    harmonize_names_sirius("1_compound_name"),
    iterations = 1000,
    check = FALSE
  )

  # Should be very fast (<0.1ms)
  expect_lt(as.numeric(median(timing$median)), 0.0001)
})

test_that("harmonize_names_sirius scales linearly", {
  skip_if_not_installed("bench")
  skip_on_cran()

  # Test with different input sizes
  small <- replicate(10, paste0(sample(1:10, 1), "_name"))
  large <- replicate(100, paste0(sample(1:10, 1), "_name"))

  time_small <- bench::mark(
    sapply(small, harmonize_names_sirius, USE.NAMES = FALSE),
    iterations = 50
  )

  time_large <- bench::mark(
    sapply(large, harmonize_names_sirius, USE.NAMES = FALSE),
    iterations = 50
  )

  # Should scale roughly linearly (within 20x for 10x data)
  ratio <- as.numeric(median(time_large$median)) / as.numeric(median(time_small$median))
  expect_lt(ratio, 20)
})
