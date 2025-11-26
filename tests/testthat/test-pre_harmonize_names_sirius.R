# Test Suite: pre_harmonize_names_sirius ----
# Tests for SIRIUS column name pre-harmonization (slash removal)

## Input Validation Tests ----

test_that("pre_harmonize_names_sirius requires input", {
  expect_error(
    pre_harmonize_names_sirius(),
    "must be provided"
  )
})

test_that("pre_harmonize_names_sirius handles NULL", {
  expect_error(
    pre_harmonize_names_sirius(NULL),
    "must be provided"
  )
})

test_that("pre_harmonize_names_sirius handles non-character input", {
  # Should convert to character
  expect_type(pre_harmonize_names_sirius(123), "character")
  expect_type(pre_harmonize_names_sirius(TRUE), "character")
})

## Basic Functionality Tests ----

test_that("pre_harmonize_names_sirius removes slash suffixes", {
  expect_equal(pre_harmonize_names_sirius("column_name/suffix"), "column_name")
  expect_equal(pre_harmonize_names_sirius("simple/extra"), "simple")
  expect_equal(pre_harmonize_names_sirius("name/info"), "name")
})

test_that("pre_harmonize_names_sirius handles no slash", {
  # Should return original if no slash
  expect_equal(pre_harmonize_names_sirius("column_name"), "column_name")
  expect_equal(pre_harmonize_names_sirius("simple"), "simple")
  expect_equal(pre_harmonize_names_sirius("name"), "name")
})

test_that("pre_harmonize_names_sirius removes from first slash", {
  # Should remove everything from FIRST slash onwards
  expect_equal(pre_harmonize_names_sirius("name/extra/info"), "name")
  expect_equal(pre_harmonize_names_sirius("col/a/b/c"), "col")
  expect_equal(pre_harmonize_names_sirius("x/y/z"), "x")
})

test_that("pre_harmonize_names_sirius handles leading slash", {
  expect_equal(pre_harmonize_names_sirius("/name"), "")
  expect_equal(pre_harmonize_names_sirius("/extra/info"), "")
})

test_that("pre_harmonize_names_sirius handles trailing slash", {
  expect_equal(pre_harmonize_names_sirius("name/"), "name")
  expect_equal(pre_harmonize_names_sirius("column/"), "column")
})

## SIRIUS-Specific Pattern Tests ----

test_that("pre_harmonize_names_sirius handles typical SIRIUS patterns", {
  # Common SIRIUS column name patterns with metadata
  expect_equal(pre_harmonize_names_sirius("molecularFormula/candidate"), "molecularFormula")
  expect_equal(pre_harmonize_names_sirius("adduct/type"), "adduct")
  expect_equal(pre_harmonize_names_sirius("SiriusScore/normalized"), "SiriusScore")
  expect_equal(pre_harmonize_names_sirius("TreeScore/raw"), "TreeScore")
})

test_that("pre_harmonize_names_sirius preserves underscores", {
  expect_equal(pre_harmonize_names_sirius("column_name/suffix"), "column_name")
  expect_equal(pre_harmonize_names_sirius("long_column_name/info"), "long_column_name")
  expect_equal(pre_harmonize_names_sirius("a_b_c/d"), "a_b_c")
})

test_that("pre_harmonize_names_sirius preserves case", {
  expect_equal(pre_harmonize_names_sirius("CamelCase/suffix"), "CamelCase")
  expect_equal(pre_harmonize_names_sirius("UPPERCASE/suffix"), "UPPERCASE")
  expect_equal(pre_harmonize_names_sirius("lowercase/suffix"), "lowercase")
})

## Edge Cases ----

test_that("pre_harmonize_names_sirius handles empty string", {
  expect_equal(pre_harmonize_names_sirius(""), "")
})

test_that("pre_harmonize_names_sirius handles only slashes", {
  expect_equal(pre_harmonize_names_sirius("/"), "")
  expect_equal(pre_harmonize_names_sirius("//"), "")
  expect_equal(pre_harmonize_names_sirius("///"), "")
})

test_that("pre_harmonize_names_sirius handles special characters before slash", {
  expect_equal(pre_harmonize_names_sirius("name@test/suffix"), "name@test")
  expect_equal(pre_harmonize_names_sirius("name.value/suffix"), "name.value")
  expect_equal(pre_harmonize_names_sirius("name-id/suffix"), "name-id")
})

test_that("pre_harmonize_names_sirius handles numbers", {
  expect_equal(pre_harmonize_names_sirius("123/suffix"), "123")
  expect_equal(pre_harmonize_names_sirius("column123/suffix"), "column123")
  expect_equal(pre_harmonize_names_sirius("1_name/2"), "1_name")
})

test_that("pre_harmonize_names_sirius handles very long names", {
  long_prefix <- paste(rep("name", 100), collapse = "_")
  input <- paste0(long_prefix, "/suffix")
  result <- pre_harmonize_names_sirius(input)

  expect_equal(result, long_prefix)
  expect_false(grepl("/", result))
})

## Vectorization Tests ----

test_that("pre_harmonize_names_sirius works with vectors", {
  input <- c("name/suffix", "simple", "col/extra")
  # Note: stringi functions are vectorized
  result <- pre_harmonize_names_sirius(input)

  expect_equal(result, c("name", "simple", "col"))
  expect_length(result, 3)
})

test_that("pre_harmonize_names_sirius handles mixed patterns in vector", {
  input <- c("name/suffix", "nosuffix", "col/a/b", "/leading")
  result <- pre_harmonize_names_sirius(input)

  expect_equal(result, c("name", "nosuffix", "col", ""))
})

## Return Value Tests ----

test_that("pre_harmonize_names_sirius always returns character", {
  expect_type(pre_harmonize_names_sirius("name/suffix"), "character")
  expect_type(pre_harmonize_names_sirius("name"), "character")
  expect_type(pre_harmonize_names_sirius(""), "character")
})

test_that("pre_harmonize_names_sirius preserves vector length", {
  expect_length(pre_harmonize_names_sirius("name"), 1)
  expect_length(pre_harmonize_names_sirius(c("a", "b", "c")), 3)
})

## Integration Tests ----

test_that("pre_harmonize_names_sirius works with real SIRIUS output", {
  # Simulate actual SIRIUS column names with metadata
  sirius_cols <- c(
    "molecularFormula/candidate",
    "adduct/type",
    "precursorFormula/normalized",
    "SiriusScore/raw",
    "TreeScore/normalized",
    "IsotopeScore/weighted",
    "numExplainedPeaks/total",
    "explainedIntensity/percentage",
    "medianMassError/ppm",
    "noSlash"
  )

  result <- pre_harmonize_names_sirius(sirius_cols)

  expect_equal(result[1], "molecularFormula")
  expect_equal(result[2], "adduct")
  expect_equal(result[4], "SiriusScore")
  expect_equal(result[10], "noSlash")
  expect_true(all(!grepl("/", result)))
})

test_that("pre_harmonize_names_sirius can be chained with harmonize_names_sirius", {
  # Typical workflow: pre-harmonize then harmonize
  input <- "1_compound_name/candidate"

  # First remove suffix
  step1 <- pre_harmonize_names_sirius(input)
  expect_equal(step1, "1_compound_name")

  # Then remove prefix (using harmonize_names_sirius)
  step2 <- harmonize_names_sirius(step1)
  expect_equal(step2, "name")
})

test_that("pre_harmonize_names_sirius can be used with dplyr rename", {
  # Simulate renaming columns
  old_names <- c("name/suffix", "id/type", "score/raw")
  new_names <- pre_harmonize_names_sirius(old_names)

  expect_equal(new_names, c("name", "id", "score"))
  expect_false(any(grepl("/", new_names)))
})

## Consistency Tests ----

test_that("pre_harmonize_names_sirius is deterministic", {
  input <- "column_name/suffix"

  result1 <- pre_harmonize_names_sirius(input)
  result2 <- pre_harmonize_names_sirius(input)
  result3 <- pre_harmonize_names_sirius(input)

  expect_identical(result1, result2)
  expect_identical(result2, result3)
})

test_that("pre_harmonize_names_sirius is idempotent", {
  input <- "name/suffix"

  # First call
  result1 <- pre_harmonize_names_sirius(input)
  # Second call on result (should be unchanged since no slash)
  result2 <- pre_harmonize_names_sirius(result1)

  expect_equal(result1, "name")
  expect_equal(result2, "name")
})

## Complementary to harmonize_names_sirius ----

test_that("pre_harmonize_names_sirius handles underscores differently", {
  # pre_harmonize removes slashes
  expect_equal(pre_harmonize_names_sirius("name_col/suffix"), "name_col")

  # harmonize removes underscore prefixes
  expect_equal(harmonize_names_sirius("1_name_col"), "col")

  # They are complementary operations
  input <- "1_name_col/suffix"
  result <- input |>
    pre_harmonize_names_sirius() |>
    harmonize_names_sirius()
  expect_equal(result, "col")
})

## Performance Tests ----

test_that("pre_harmonize_names_sirius is reasonably fast", {
  skip_if_not_installed("bench")

  timing <- bench::mark(
    pre_harmonize_names_sirius("column_name/suffix"),
    iterations = 1000,
    check = FALSE
  )

  # Should be very fast (<0.1ms)
  expect_lt(as.numeric(median(timing$median)), 0.0001)
})

test_that("pre_harmonize_names_sirius handles large vectors efficiently", {
  skip_on_cran()

  # Create large vector
  large_input <- rep(c("name/suffix", "col/extra", "simple"), 1000)

  # Should complete quickly
  result <- pre_harmonize_names_sirius(large_input)

  expect_length(result, 3000)
  expect_true(all(!grepl("/", result)))
})
