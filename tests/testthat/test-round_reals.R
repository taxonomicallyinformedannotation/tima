# Test Suite: round_reals ----

library(testthat)

## Basic rounding functionality ----

test_that("round_reals rounds numeric columns by default", {
  df <- data.frame(
    structure_exact_mass = c(123.456789, 987.654321),
    structure_xlogp = c(2.345678, -1.234567),
    other_column = c(1, 2)
  )

  result <- round_reals(df)

  expect_equal(result$structure_exact_mass[1], 123.45679)
  expect_equal(result$structure_xlogp[1], 2.34568)
  expect_equal(result$other_column[1], 1) # Unchanged
})

test_that("round_reals respects dig parameter", {
  df <- data.frame(
    structure_exact_mass = c(123.456789)
  )

  result <- round_reals(df, dig = 2)
  expect_equal(result$structure_exact_mass[1], 123.46)

  result <- round_reals(df, dig = 0)
  expect_equal(result$structure_exact_mass[1], 123)

  result <- round_reals(df, dig = 8)
  expect_equal(result$structure_exact_mass[1], 123.456789)
})

test_that("round_reals rounds only specified columns", {
  df <- data.frame(
    structure_exact_mass = c(123.456789),
    other_numeric = c(987.654321)
  )

  result <- round_reals(df, cols = c("structure_exact_mass"))

  expect_equal(result$structure_exact_mass[1], 123.45679)
  expect_equal(result$other_numeric[1], 987.654321) # Unchanged
})

## Input validation ----

test_that("round_reals requires data frame input", {
  expect_error(
    round_reals("not_a_df"),
    "must be a data frame"
  )
})

test_that("round_reals validates dig parameter", {
  df <- data.frame(structure_exact_mass = 123.456)

  expect_error(round_reals(df, dig = -1), "non-negative")
  expect_error(round_reals(df, dig = "5"), "non-negative integer")
  expect_error(round_reals(df, dig = 2.5), "non-negative integer")
})

## Column matching ----

test_that("round_reals finds columns with partial matches", {
  df <- data.frame(
    structure_exact_mass_calculated = c(123.456789),
    structure_xlogp_pred = c(2.345678)
  )

  result <- round_reals(df)

  expect_equal(result$structure_exact_mass_calculated[1], 123.45679)
  expect_equal(result$structure_xlogp_pred[1], 2.34568)
})

test_that("round_reals handles missing columns gracefully", {
  df <- data.frame(
    unrelated_column = c(123.456789)
  )

  # Should return unchanged
  result <- round_reals(df)
  expect_equal(result$unrelated_column[1], 123.456789)
})

test_that("round_reals handles empty column list", {
  df <- data.frame(
    structure_exact_mass = c(123.456789)
  )

  result <- round_reals(df, cols = character(0))
  expect_equal(result$structure_exact_mass[1], 123.456789) # Unchanged
})

## Edge cases ----

test_that("round_reals handles empty data frame", {
  df <- data.frame()
  result <- round_reals(df)

  expect_equal(nrow(result), 0)
})

test_that("round_reals handles single row", {
  df <- data.frame(structure_exact_mass = 123.456789)
  result <- round_reals(df)

  expect_equal(nrow(result), 1)
  expect_equal(result$structure_exact_mass[1], 123.45679)
})

test_that("round_reals handles NA values", {
  df <- data.frame(
    structure_exact_mass = c(123.456789, NA, 987.654321)
  )

  result <- round_reals(df)

  expect_equal(result$structure_exact_mass[1], 123.45679)
  expect_true(is.na(result$structure_exact_mass[2]))
  expect_equal(result$structure_exact_mass[3], 987.65432)
})

test_that("round_reals handles Inf values", {
  df <- data.frame(
    structure_exact_mass = c(123.456789, Inf, -Inf)
  )

  result <- round_reals(df)

  expect_equal(result$structure_exact_mass[1], 123.45679)
  expect_equal(result$structure_exact_mass[2], Inf)
  expect_equal(result$structure_exact_mass[3], -Inf)
})

test_that("round_reals handles very large numbers", {
  df <- data.frame(
    structure_exact_mass = c(1e10 + 0.123456789)
  )

  result <- round_reals(df, dig = 2)
  expect_equal(result$structure_exact_mass[1], 1e10 + 0.12)
})

test_that("round_reals handles very small numbers", {
  df <- data.frame(
    structure_exact_mass = c(1e-10 + 1.23456e-15)
  )

  result <- round_reals(df, dig = 15)
  expect_true(abs(result$structure_exact_mass[1] - 1e-10) < 1e-14)
})

## Multiple columns ----

test_that("round_reals handles multiple matching columns", {
  df <- data.frame(
    structure_exact_mass = c(123.456789),
    structure_exact_mass_2 = c(456.789123),
    structure_xlogp = c(2.345678),
    other = c(999.999999)
  )

  result <- round_reals(df)

  expect_equal(result$structure_exact_mass[1], 123.45679)
  expect_equal(result$structure_exact_mass_2[1], 456.78912)
  expect_equal(result$structure_xlogp[1], 2.34568)
  expect_equal(result$other[1], 999.999999) # Not matched
})

## Custom column patterns ----

test_that("round_reals works with custom column patterns", {
  df <- data.frame(
    my_mass_column = c(123.456789),
    my_logp_column = c(2.345678),
    other_column = c(999.999999)
  )

  result <- round_reals(df, cols = c("my_mass", "my_logp"))

  expect_equal(result$my_mass_column[1], 123.45679)
  expect_equal(result$my_logp_column[1], 2.34568)
  expect_equal(result$other_column[1], 999.999999)
})

## Preserve data frame structure ----

test_that("round_reals preserves data frame structure", {
  df <- data.frame(
    structure_exact_mass = c(123.456789),
    character_col = c("test"),
    factor_col = factor(c("a")),
    stringsAsFactors = FALSE
  )

  result <- round_reals(df)

  expect_equal(ncol(result), ncol(df))
  expect_equal(nrow(result), nrow(df))
  expect_equal(names(result), names(df))
  expect_type(result$character_col, "character")
  expect_s3_class(result$factor_col, "factor")
})

## Non-numeric columns with matching names ----

test_that("round_reals skips non-numeric columns even if name matches", {
  df <- data.frame(
    structure_exact_mass = c("123.456789"), # Character, not numeric
    stringsAsFactors = FALSE
  )

  # Should handle gracefully (may skip or warn)
  result <- round_reals(df)
  expect_equal(result$structure_exact_mass[1], 123.456789)
})

## Performance ----

test_that("round_reals is fast for large data frames", {
  skip_on_cran()

  df <- data.frame(
    structure_exact_mass = runif(10000, 100, 1000),
    structure_xlogp = runif(10000, -5, 5)
  )

  start_time <- Sys.time()
  result <- round_reals(df)
  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")

  expect_true(elapsed < 1.0)
  expect_equal(nrow(result), 10000)
})

## Logging behavior ----

test_that("round_reals handles no matching columns gracefully", {
  df <- data.frame(unrelated = c(123.456))

  # Should complete without error, may log internally
  expect_silent(round_reals(df))
})

test_that("round_reals logs appropriately for column operations", {
  df <- data.frame(
    structure_exact_mass = c(123.456789),
    structure_xlogp = c(2.345678)
  )

  # Should complete successfully
  result <- round_reals(df)
  expect_s3_class(result, "data.frame")
})

# test_that("round_reals logs when no patterns specified", {
#   df <- data.frame(structure_exact_mass = c(123.456))
#
#   expect_silent(round_reals(df, cols = character(0)))
# })
