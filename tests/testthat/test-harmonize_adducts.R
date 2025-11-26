# Test Suite: harmonize_adducts ----

library(testthat)

## Setup ----

# Sample translations for testing
SAMPLE_TRANSLATIONS <- c(
  "M+H" = "[M+H]+",
  "(M+H)+" = "[M+H]+",
  "M+Na" = "[M+Na]+",
  "(M-H)-" = "[M-H]-",
  "M-H" = "[M-H]-",
  "[M+2H]2" = "[M+2H]2+"
)

## Input Validation Tests ----

test_that("harmonize_adducts validates data frame", {
  trans <- c("M+H" = "[M+H]+")

  expect_error(
    harmonize_adducts(df = "not_a_df", adducts_translations = trans),
    "data.*frame"
  )

  expect_error(
    harmonize_adducts(df = NULL, adducts_translations = trans),
    class = "error"
  )

  expect_error(
    harmonize_adducts(df = list(a = 1), adducts_translations = trans),
    "data.*frame"
  )
})

test_that("harmonize_adducts validates column name", {
  df <- data.frame(adduct = c("M+H"))
  trans <- c("M+H" = "[M+H]+")

  expect_error(
    harmonize_adducts(df, adducts_colname = NULL, adducts_translations = trans),
    "non-NULL"
  )

  expect_error(
    harmonize_adducts(df, adducts_colname = "", adducts_translations = trans),
    class = "error"
  )

  expect_error(
    harmonize_adducts(
      df,
      adducts_colname = c("a", "b"),
      adducts_translations = trans
    ),
    "length-1"
  )
})

test_that("harmonize_adducts validates translations structure", {
  df <- data.frame(adduct = c("M+H"))

  # Not a character vector
  expect_error(
    harmonize_adducts(df, adducts_translations = list("M+H" = "[M+H]+")),
    "character vector"
  )

  # Unnamed vector
  expect_error(
    harmonize_adducts(df, adducts_translations = c("[M+H]+", "[M+Na]+")),
    "named vector"
  )

  expect_error(
    harmonize_adducts(df, adducts_translations = 123),
    "character vector"
  )
})

## Early Return Tests ----

test_that("harmonize_adducts returns unchanged if column missing", {
  df <- data.frame(other_col = c("a", "b", "c"))
  trans <- c("M+H" = "[M+H]+")

  result <- harmonize_adducts(
    df,
    adducts_colname = "adduct",
    adducts_translations = trans
  )

  expect_identical(result, df)
})

test_that("harmonize_adducts returns unchanged if no translations provided", {
  df <- data.frame(adduct = c("M+H", "M+Na"))

  # Missing parameter
  result1 <- harmonize_adducts(df)
  expect_identical(result1, df)

  # Empty vector
  result2 <- harmonize_adducts(df, adducts_translations = character(0))
  expect_identical(result2, df)
})

## Harmonization Tests ----

test_that("harmonize_adducts replaces single variation", {
  df <- data.frame(adduct = c("M+H", "M+H", "M+H"))
  trans <- c("M+H" = "[M+H]+")

  result <- harmonize_adducts(df, adducts_translations = trans)

  expect_equal(result$adduct, c("[M+H]+", "[M+H]+", "[M+H]+"))
})

test_that("harmonize_adducts replaces multiple variations", {
  df <- data.frame(adduct = c("M+H", "M+Na", "M-H"))
  trans <- c(
    "M+H" = "[M+H]+",
    "M+Na" = "[M+Na]+",
    "M-H" = "[M-H]-"
  )

  result <- harmonize_adducts(df, adducts_translations = trans)

  expect_equal(result$adduct, c("[M+H]+", "[M+Na]+", "[M-H]-"))
})

test_that("harmonize_adducts handles multiple forms mapping to same standard", {
  df <- data.frame(adduct = c("M+H", "(M+H)+", "[M+H]+"))
  trans <- c(
    "M+H" = "[M+H]+",
    "(M+H)+" = "[M+H]+"
  )

  result <- harmonize_adducts(df, adducts_translations = trans)

  # All should become [M+H]+
  expect_true(all(result$adduct == "[M+H]+"))
  expect_equal(length(unique(result$adduct)), 1)
})

test_that("harmonize_adducts preserves non-matching adducts", {
  df <- data.frame(adduct = c("M+H", "[M+K]+", "M+Na"))
  trans <- c("M+H" = "[M+H]+")

  result <- harmonize_adducts(df, adducts_translations = trans)

  # Only M+H should be changed
  expect_equal(result$adduct, c("[M+H]+", "[M+K]+", "M+Na"))
})

test_that("harmonize_adducts works with custom column name", {
  df <- data.frame(my_adduct_col = c("M+H", "M+Na"))
  trans <- c("M+H" = "[M+H]+", "M+Na" = "[M+Na]+")

  result <- harmonize_adducts(
    df,
    adducts_colname = "my_adduct_col",
    adducts_translations = trans
  )

  expect_equal(result$my_adduct_col, c("[M+H]+", "[M+Na]+"))
})

## Data Integrity Tests ----

test_that("harmonize_adducts preserves other columns", {
  df <- data.frame(
    feature_id = c("FT001", "FT002", "FT003"),
    adduct = c("M+H", "M+Na", "M-H"),
    mz = c(100.5, 200.3, 300.7)
  )
  trans <- c("M+H" = "[M+H]+")

  result <- harmonize_adducts(df, adducts_translations = trans)

  expect_equal(result$feature_id, df$feature_id)
  expect_equal(result$mz, df$mz)
  expect_equal(ncol(result), ncol(df))
})

test_that("harmonize_adducts preserves row order", {
  df <- data.frame(
    id = 1:5,
    adduct = c("M+H", "M+Na", "M+H", "M-H", "M+H")
  )
  trans <- c("M+H" = "[M+H]+")

  result <- harmonize_adducts(df, adducts_translations = trans)

  expect_equal(result$id, df$id)
  expect_equal(nrow(result), nrow(df))
})

test_that("harmonize_adducts handles NA values correctly", {
  df <- data.frame(adduct = c("M+H", NA, "M+Na", NA))
  trans <- c("M+H" = "[M+H]+", "M+Na" = "[M+Na]+")

  result <- harmonize_adducts(df, adducts_translations = trans)

  expect_equal(result$adduct[1], "[M+H]+")
  expect_true(is.na(result$adduct[2]))
  expect_equal(result$adduct[3], "[M+Na]+")
  expect_true(is.na(result$adduct[4]))
})

test_that("harmonize_adducts handles empty data frame", {
  df <- data.frame(adduct = character(0))
  trans <- c("M+H" = "[M+H]+")

  result <- harmonize_adducts(df, adducts_translations = trans)

  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 1)
  expect_true("adduct" %in% names(result))
})

## Edge Cases ----

test_that("harmonize_adducts handles large data frames", {
  skip_on_cran()

  df <- data.frame(adduct = rep(c("M+H", "M+Na", "M-H"), 1000))
  trans <- SAMPLE_TRANSLATIONS

  result <- harmonize_adducts(df, adducts_translations = trans)

  expect_equal(nrow(result), 3000)
  expect_true(all(result$adduct %in% c("[M+H]+", "[M+Na]+", "[M-H]-")))
})

test_that("harmonize_adducts handles special characters in adducts", {
  df <- data.frame(adduct = c("M+H-H2O", "M+NH4"))
  trans <- c(
    "M+H-H2O" = "[M+H-H2O]+",
    "M+NH4" = "[M+NH4]+"
  )

  result <- harmonize_adducts(df, adducts_translations = trans)

  expect_equal(result$adduct, c("[M+H-H2O]+", "[M+NH4]+"))
})

test_that("harmonize_adducts handles parentheses in original", {
  df <- data.frame(adduct = c("(M+H)+", "(M+Na)+", "(M-H)-"))
  trans <- c(
    "(M+H)+" = "[M+H]+",
    "(M+Na)+" = "[M+Na]+",
    "(M-H)-" = "[M-H]-"
  )

  result <- harmonize_adducts(df, adducts_translations = trans)

  expect_true(all(grepl("^\\[", result$adduct))) # All start with [
})

test_that("harmonize_adducts handles brackets in original", {
  df <- data.frame(adduct = c("[M+H]", "[M+Na]"))
  trans <- c(
    "[M+H]" = "[M+H]+",
    "[M+Na]" = "[M+Na]+"
  )

  result <- harmonize_adducts(df, adducts_translations = trans)

  expect_equal(result$adduct, c("[M+H]+", "[M+Na]+"))
})

## Tidytable Compatibility ----

test_that("harmonize_adducts works with tidytable", {
  df <- tidytable::tidytable(adduct = c("M+H", "M+Na", "M-H"))
  trans <- c("M+H" = "[M+H]+", "M+Na" = "[M+Na]+")

  result <- harmonize_adducts(df, adducts_translations = trans)

  expect_s3_class(result, "tidytable")
  expect_equal(result$adduct[1:2], c("[M+H]+", "[M+Na]+"))
})

## Helper Function Tests ----

test_that("count_unique_values counts correctly", {
  expect_equal(count_unique_values(c(1, 2, 3)), 3)
  expect_equal(count_unique_values(c(1, 1, 1)), 1)
  expect_equal(count_unique_values(c(1, NA, 2, NA, 3)), 3)
  expect_equal(count_unique_values(c("a", "b", "a")), 2)
  expect_equal(count_unique_values(character(0)), 0)
})

test_that("validate_adduct_translations works correctly", {
  # Valid
  expect_silent(validate_adduct_translations(c("a" = "b")))
  expect_silent(validate_adduct_translations(c("a" = "b", "c" = "d")))

  # Invalid
  expect_error(
    validate_adduct_translations(list("a" = "b")),
    "character vector"
  )
  expect_error(
    validate_adduct_translations(c("a", "b")),
    "named vector"
  )
})

## Return Value Tests ----

test_that("harmonize_adducts returns data frame", {
  df <- data.frame(adduct = c("M+H"))
  trans <- c("M+H" = "[M+H]+")

  result <- harmonize_adducts(df, adducts_translations = trans)

  expect_s3_class(result, "data.frame")
})

test_that("harmonize_adducts preserves data frame class", {
  # Regular data.frame
  df1 <- data.frame(adduct = c("M+H"))
  result1 <- harmonize_adducts(df1, adducts_translations = c("M+H" = "[M+H]+"))
  expect_s3_class(result1, "data.frame")

  # tidytable
  df2 <- tidytable::tidytable(adduct = c("M+H"))
  result2 <- harmonize_adducts(df2, adducts_translations = c("M+H" = "[M+H]+"))
  expect_s3_class(result2, "tidytable")
})

## Integration Tests ----

test_that("harmonize_adducts works in a typical workflow", {
  # Simulate data from different MS tools
  df <- tidytable::tidytable(
    feature_id = paste0("FT", 1:10),
    mz = runif(10, 100, 500),
    adduct = c(
      "M+H",
      "(M+H)+",
      "[M+H]+", # Should all become [M+H]+
      "M+Na",
      "(M+Na)+", # Should all become [M+Na]+
      "M-H",
      "(M-H)-", # Should all become [M-H]-
      "[M+2H]2",
      "M+K",
      "(M+K)+" # Mixed
    )
  )

  trans <- c(
    "M+H" = "[M+H]+",
    "(M+H)+" = "[M+H]+",
    "M+Na" = "[M+Na]+",
    "(M+Na)+" = "[M+Na]+",
    "M-H" = "[M-H]-",
    "(M-H)-" = "[M-H]-",
    "[M+2H]2" = "[M+2H]2+",
    "M+K" = "[M+K]+",
    "(M+K)+" = "[M+K]+"
  )

  result <- harmonize_adducts(df, adducts_translations = trans)

  # Check standardization
  expect_true(all(grepl("^\\[", result$adduct))) # All start with [
  expect_true(all(grepl("[+-]$", result$adduct))) # All end with + or -

  # Check specific cases
  expect_equal(result$adduct[1:3], rep("[M+H]+", 3))
  expect_equal(result$adduct[4:5], rep("[M+Na]+", 2))
  expect_equal(result$adduct[6:7], rep("[M-H]-", 2))
})

## Performance Tests ----

test_that("harmonize_adducts is reasonably fast", {
  skip_if_not_installed("bench")
  skip_on_cran()

  df <- data.frame(adduct = rep(c("M+H", "M+Na", "M-H"), 100))
  trans <- SAMPLE_TRANSLATIONS

  timing <- system.time(replicate(
    n = 100L,
    expr = harmonize_adducts(df, adducts_translations = trans)
  ))

  # Should complete quickly
  expect_lt(timing["elapsed"], 0.1)
})
