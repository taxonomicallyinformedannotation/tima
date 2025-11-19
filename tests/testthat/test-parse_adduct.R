# Test Suite: parse_adduct ----

library(testthat)

## Fixtures ----

# Expected return structure for all valid parses
EXPECTED_NAMES <- c("n_mer", "n_iso", "los_add_clu", "n_charges", "charge")

# Common adducts for parametric testing
COMMON_POSITIVE_ADDUCTS <- c(
  "[M+H]+",
  "[M+Na]+",
  "[M+K]+",
  "[M+NH4]+"
)

COMMON_NEGATIVE_ADDUCTS <- c(
  "[M-H]-",
  "[M+Cl]-",
  "[M+HCOO]-"
)

## Internal Utility Helpers ----

#' Helper: Validate parse_adduct result structure
#'
#' @param result Result from parse_adduct()
#' @param expected_names Expected names in result vector
expect_valid_parse_result <- function(result, expected_names = EXPECTED_NAMES) {
  expect_type(result, "double")
  expect_named(result, expected_names)
  expect_length(result, length(expected_names))
}

#' Helper: Check if parse failed (all zeros)
#'
#' @param result Result from parse_adduct()
#' @return Logical indicating if parsing failed
is_parse_failed <- function(result) {
  all(result == 0)
}

## Input Validation ----

test_that("parse_adduct handles NULL input gracefully", {
  result <- parse_adduct(NULL)

  expect_valid_parse_result(result)
  expect_equal(unname(result["n_mer"]), 0)
  expect_equal(unname(result["n_iso"]), 0)
  expect_equal(unname(result["n_charges"]), 0)
  expect_equal(unname(result["charge"]), 0)
})

test_that("parse_adduct handles empty string without error", {
  result <- parse_adduct("")

  expect_valid_parse_result(result)
  expect_equal(unname(result["n_mer"]), 0)
})

test_that("parse_adduct returns zeros for invalid format", {
  result <- parse_adduct("invalid_format")

  expect_valid_parse_result(result)
  expect_equal(unname(result["n_mer"]), 0)
})

test_that("parse_adduct handles malformed brackets", {
  result <- parse_adduct("M+H]+")

  expect_valid_parse_result(result)
  expect_equal(unname(result["n_mer"]), 0)
})

## Basic Adduct Parsing ----

test_that("parse_adduct extracts components from simple positive adduct [M+H]+", {
  result <- parse_adduct("[M+H]+")

  expect_valid_parse_result(result)
  expect_equal(unname(result["n_mer"]), 1, info = "Monomer count")
  expect_equal(unname(result["n_iso"]), 0, info = "No isotope shift")
  expect_equal(unname(result["n_charges"]), 1, info = "Single charge")
  expect_equal(unname(result["charge"]), 1, info = "Positive mode")
  expect_true(unname(result["los_add_clu"]) > 0, info = "H adds mass")
})

test_that("parse_adduct extracts components from simple negative adduct [M-H]-", {
  result <- parse_adduct("[M-H]-")

  expect_valid_parse_result(result)
  expect_equal(unname(result["n_mer"]), 1, info = "Monomer count")
  expect_equal(unname(result["n_iso"]), 0, info = "No isotope shift")
  expect_equal(unname(result["n_charges"]), 1, info = "Single charge")
  expect_equal(unname(result["charge"]), -1, info = "Negative mode")
  expect_true(unname(result["los_add_clu"]) < 0, info = "H loss reduces mass")
})

test_that("parse_adduct calculates correct mass for sodium adduct [M+Na]+", {
  result <- parse_adduct("[M+Na]+")

  expect_valid_parse_result(result)
  expect_equal(unname(result["n_mer"]), 1)
  expect_equal(unname(result["charge"]), 1)
  # Na is approximately 23 Da
  expect_true(unname(result["los_add_clu"]) > 20, info = "Na adds ~23 Da")
  expect_true(unname(result["los_add_clu"]) < 25, info = "Na adds ~23 Da")
})

test_that("parse_adduct works for all common positive adducts", {
  # Parametric test for common positive mode adducts
  for (adduct in COMMON_POSITIVE_ADDUCTS) {
    result <- parse_adduct(adduct)

    expect_valid_parse_result(result)
    expect_equal(
      unname(result["charge"]),
      1,
      info = paste(adduct, "- positive mode")
    )
    expect_true(
      unname(result["n_mer"]) >= 1,
      info = paste(adduct, "- has monomers")
    )
  }
})

test_that("parse_adduct works for all common negative adducts", {
  # Parametric test for common negative mode adducts
  for (adduct in COMMON_NEGATIVE_ADDUCTS) {
    result <- parse_adduct(adduct)

    expect_valid_parse_result(result)
    expect_equal(
      unname(result["charge"]),
      -1,
      info = paste(adduct, "- negative mode")
    )
    expect_true(
      unname(result["n_mer"]) >= 1,
      info = paste(adduct, "- has monomers")
    )
  }
})

## Multimer Parsing ----

test_that("parse_adduct extracts multimer count from dimer notation [2M+H]+", {
  result <- parse_adduct("[2M+H]+")

  expect_valid_parse_result(result)
  expect_equal(unname(result["n_mer"]), 2, info = "Dimer has 2 monomers")
  expect_equal(unname(result["charge"]), 1)
})

test_that("parse_adduct extracts multimer count from trimer notation [3M+H]+", {
  result <- parse_adduct("[3M+H]+")

  expect_valid_parse_result(result)
  expect_equal(unname(result["n_mer"]), 3, info = "Trimer has 3 monomers")
})

test_that("parse_adduct handles higher-order multimers correctly", {
  # Test multimers up to pentamer
  for (n in 4:5) {
    adduct <- sprintf("[%dM+Na]+", n)
    result <- parse_adduct(adduct)

    expect_valid_parse_result(result)
    expect_equal(
      unname(result["n_mer"]),
      n,
      info = paste(adduct, "multimer count")
    )
  }
})

test_that("parse_adduct defaults to monomer when multimer not specified", {
  result <- parse_adduct("[M+H]+")

  expect_equal(unname(result["n_mer"]), 1, info = "Default multimer is 1")
})

## Isotope Parsing ----

test_that("parse_adduct extracts isotope shift from M+1 isotopologue", {
  result <- parse_adduct("[M1+H]+")

  expect_valid_parse_result(result)
  expect_equal(unname(result["n_iso"]), 1, info = "M+1 isotope shift")
  expect_equal(unname(result["n_mer"]), 1)
})

test_that("parse_adduct extracts isotope shift from M+2 isotopologue", {
  result <- parse_adduct("[M2+H]+")

  expect_valid_parse_result(result)
  expect_equal(unname(result["n_iso"]), 2, info = "M+2 isotope shift")
})

test_that("parse_adduct extracts isotope shift from M+3 isotopologue", {
  result <- parse_adduct("[M3+Na]+")

  expect_valid_parse_result(result)
  expect_equal(unname(result["n_iso"]), 3, info = "M+3 isotope shift")
})

test_that("parse_adduct defaults to zero isotope shift when not specified", {
  result <- parse_adduct("[M+H]+")

  expect_equal(unname(result["n_iso"]), 0, info = "Default isotope shift is 0")
})

## Multiple Charge State ----

test_that("parse_adduct extracts doubly charged state from [M+2H]2+", {
  result <- parse_adduct("[M+2H]2+")

  expect_valid_parse_result(result)
  expect_equal(unname(result["n_charges"]), 2, info = "2+ charge state")
  expect_equal(unname(result["charge"]), 1, info = "Positive polarity")
})

test_that("parse_adduct extracts triply charged state from [M+3H]3+", {
  result <- parse_adduct("[M+3H]3+")

  expect_valid_parse_result(result)
  expect_equal(unname(result["n_charges"]), 3, info = "3+ charge state")
  expect_equal(unname(result["charge"]), 1, info = "Positive polarity")
})

test_that("parse_adduct handles doubly negative charge [M-2H]2-", {
  result <- parse_adduct("[M-2H]2-")

  expect_valid_parse_result(result)
  expect_equal(unname(result["n_charges"]), 2, info = "2- charge state")
  expect_equal(unname(result["charge"]), -1, info = "Negative polarity")
})

test_that("parse_adduct defaults to single charge when not specified", {
  result <- parse_adduct("[M+H]+")

  expect_equal(
    unname(result["n_charges"]),
    1,
    info = "Default charge count is 1"
  )
})

## Complex Modification ----

test_that("parse_adduct calculates mass change for water loss [M+H-H2O]+", {
  result <- parse_adduct("[M+H-H2O]+")

  expect_valid_parse_result(result)
  expect_equal(unname(result["n_mer"]), 1)
  expect_equal(unname(result["n_charges"]), 1)
  # H minus H2O should be negative overall (net loss of ~17 Da)
  expect_true(
    unname(result["los_add_clu"]) < 1,
    info = "Water loss reduces mass"
  )
})

test_that("parse_adduct calculates mass change for ammonia loss [M+H-NH3]+", {
  result <- parse_adduct("[M+H-NH3]+")

  expect_valid_parse_result(result)
  expect_equal(unname(result["n_mer"]), 1)
  # H minus NH3 should be negative (net loss of ~16 Da)
  expect_true(
    unname(result["los_add_clu"]) < 0,
    info = "Ammonia loss reduces mass"
  )
})

test_that("parse_adduct calculates mass change for multiple additions [M+Na+K]+", {
  result <- parse_adduct("[M+Na+K]+")

  expect_valid_parse_result(result)
  # Na (~23 Da) + K (~39 Da) = ~62 Da
  expect_true(
    unname(result["los_add_clu"]) > 60,
    info = "Na+K adds ~62 Da"
  )
})

test_that("parse_adduct ignores comments in parentheses", {
  result_with_comment <- parse_adduct("[M-C6H12O6 (hexose)+H]+")
  result_without_comment <- parse_adduct("[M-C6H12O6+H]+")

  # Results should be equivalent after removing comment
  expect_equal(
    unname(result_with_comment["los_add_clu"]),
    unname(result_without_comment["los_add_clu"]),
    tolerance = 1e-6,
    info = "Parenthetical comments should be ignored"
  )
})

## Combined Complex Pattern ----

test_that("parse_adduct handles dimer with isotope [2M1+H]+", {
  result <- parse_adduct("[2M1+H]+")

  expect_valid_parse_result(result)
  expect_equal(unname(result["n_mer"]), 2, info = "Dimer")
  expect_equal(unname(result["n_iso"]), 1, info = "M+1 isotope")
  expect_equal(unname(result["charge"]), 1)
})

test_that("parse_adduct handles doubly charged with modification", {
  result <- parse_adduct("[M+2H-H2O]2+")

  expect_valid_parse_result(result)
  expect_equal(unname(result["n_charges"]), 2, info = "Doubly charged")
  # 2H minus H2O should be small positive or negative
  expect_true(abs(unname(result["los_add_clu"])) < 20)
})

test_that("parse_adduct handles very complex adduct notation", {
  result <- parse_adduct("[2M1-C6H12O6+NaCl+H]2+")

  expect_valid_parse_result(result)
  expect_equal(unname(result["n_mer"]), 2, info = "Dimer")
  expect_equal(unname(result["n_iso"]), 1, info = "M+1 isotope")
  expect_equal(unname(result["n_charges"]), 2, info = "Doubly charged")
  expect_equal(unname(result["charge"]), 1, info = "Positive mode")
})

test_that("parse_adduct handles chloride adduct in negative mode", {
  result <- parse_adduct("[M+Cl]-")

  expect_valid_parse_result(result)
  expect_equal(unname(result["charge"]), -1, info = "Negative mode")
  # Cl is approximately 35 Da
  expect_true(unname(result["los_add_clu"]) > 30, info = "Cl adds ~35 Da")
  expect_true(unname(result["los_add_clu"]) < 40, info = "Cl adds ~35 Da")
})

## Edge Cases and Special Formats ----

test_that("parse_adduct handles alternative adduct notations with slash", {
  # Some databases use slash to indicate alternative adducts
  result <- parse_adduct("[M+H]+/[M+Na]+")

  expect_valid_parse_result(result)
  # Should successfully parse at least one alternative
  expect_true(
    unname(result["n_mer"]) > 0,
    info = "Should parse first alternative"
  )
})

test_that("parse_adduct handles alternative adduct notations with pipe", {
  result <- parse_adduct("[M+H]+|[M+K]+")

  expect_valid_parse_result(result)
  expect_true(
    unname(result["n_mer"]) > 0,
    info = "Should parse first alternative"
  )
})

## Performance and Consistency ----

test_that("parse_adduct returns consistent results for same input", {
  adduct <- "[M+H]+"

  result1 <- parse_adduct(adduct)
  result2 <- parse_adduct(adduct)

  expect_identical(result1, result2, info = "Results should be deterministic")
})

test_that("parse_adduct is case-sensitive for element symbols", {
  # This documents current behavior - may want to add case handling later
  result_lowercase <- parse_adduct("[M+na]+")
  result_uppercase <- parse_adduct("[M+Na]+")

  # These might differ if lowercase 'na' isn't recognized as sodium
  expect_valid_parse_result(result_lowercase)
  expect_valid_parse_result(result_uppercase)
})

test_that("parse_adduct handles formate adduct in negative mode", {
  result <- parse_adduct("[M+HCOO]-")

  expect_valid_parse_result(result)
  expect_equal(unname(result["charge"]), -1, info = "Negative mode")
  # Formate is approximately 45 Da
  expect_true(unname(result["los_add_clu"]) > 40, info = "Formate adds ~45 Da")
})

test_that("parse_adduct handles acetate adduct in negative mode", {
  result <- parse_adduct("[M+CH3COO]-")

  expect_valid_parse_result(result)
  expect_equal(unname(result["charge"]), -1, info = "Negative mode")
  # Acetate is approximately 59 Da
  expect_true(unname(result["los_add_clu"]) > 50, info = "Acetate adds ~59 Da")
})

## Consistency and Return Value ----

test_that("parse_adduct gives consistent results for equivalent notations", {
  result_implicit <- parse_adduct("[M+H]+")
  result_explicit <- parse_adduct("[1M+H]+")

  expect_equal(
    unname(result_implicit["n_mer"]),
    unname(result_explicit["n_mer"]),
    info = "[M+H]+ and [1M+H]+ should be equivalent"
  )
})

test_that("parse_adduct always returns named numeric vector of length 5", {
  result <- parse_adduct("[M+H]+")

  expect_length(result, 5)
  expect_named(result)
  expect_type(result, "double")
})

# Logging and Error Behavior ----

test_that("parse_adduct returns failed result for invalid input", {
  result <- parse_adduct("bad_format")

  expect_true(is_parse_failed(result))
})

test_that("parse_adduct handles successful parse without error", {
  # Should not throw errors or warnings for valid input
  expect_no_error(parse_adduct("[M+H]+"))
})

## Performance ----

test_that("parse_adduct is fast enough for batch processing", {
  skip_on_cran()

  adducts <- rep("[M+H]+", 1000)

  start_time <- Sys.time()
  results <- lapply(adducts, parse_adduct)
  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")

  expect_true(elapsed < 1.0, info = "1000 parses should complete in < 1 second")
  expect_equal(length(results), 1000, info = "Should process all inputs")
})

## Additional Coverage: Uncommon but Valid Adducts ----

test_that("parse_adduct handles ammonium adduct [M+NH4]+", {
  result <- parse_adduct("[M+NH4]+")

  expect_valid_parse_result(result)
  expect_equal(unname(result["charge"]), 1)
  # NH4 is approximately 18 Da
  expect_true(unname(result["los_add_clu"]) > 15)
})

test_that("parse_adduct handles potassium adduct [M+K]+", {
  result <- parse_adduct("[M+K]+")

  expect_valid_parse_result(result)
  expect_equal(unname(result["charge"]), 1)
  # K is approximately 39 Da
  expect_true(unname(result["los_add_clu"]) > 35)
  expect_true(unname(result["los_add_clu"]) < 45)
})

test_that("parse_adduct handles CO2 loss [M-CO2+H]+", {
  result <- parse_adduct("[M-CO2+H]+")

  expect_valid_parse_result(result)
  expect_equal(unname(result["charge"]), 1)
  # H - CO2 should be negative (net loss of ~43 Da)
  expect_true(unname(result["los_add_clu"]) < 0)
})

## Comments with Special Characters ----

test_that("parse_adduct handles comments with slash inside parentheses", {
  # This tests the fix for slash inside parenthetical comments
  # which should NOT be treated as alternative adduct delimiter
  result_with_slash_comment <- parse_adduct(
    "[M-C6H10O4 (methylpentose/desoxyhexose-H2O)+H]+"
  )
  result_without_comment <- parse_adduct("[M-C6H10O4+H]+")

  expect_valid_parse_result(result_with_slash_comment)
  # Should successfully parse (not all zeros)
  expect_false(
    is_parse_failed(result_with_slash_comment),
    info = "Should parse adduct with slash in comment"
  )
  # Should have same mass change as version without comment
  expect_equal(
    unname(result_with_slash_comment["los_add_clu"]),
    unname(result_without_comment["los_add_clu"]),
    tolerance = 1e-6,
    info = "Comments should be ignored in mass calculation"
  )
  # Should be in positive mode with 1 charge
  expect_equal(unname(result_with_slash_comment["charge"]), 1)
  expect_equal(unname(result_with_slash_comment["n_charges"]), 1)
})

test_that("parse_adduct handles complex sugar loss with slash in comment", {
  # Another real-world example from the codebase
  result <- parse_adduct(
    "[M-C12H20O8 (2xmethylpentose/desoxyhexose-H2O)+H]+"
  )

  expect_valid_parse_result(result)
  expect_false(
    is_parse_failed(result),
    info = "Should parse adduct with slash in comment"
  )
  expect_equal(unname(result["n_mer"]), 1)
  expect_equal(unname(result["charge"]), 1)
  # Loss of C12H20O8 plus gain of H should be net negative
  expect_true(
    unname(result["los_add_clu"]) < 0,
    info = "Large sugar loss should result in negative mass change"
  )
})

test_that("parse_adduct distinguishes between slash in comment vs slash as delimiter", {
  # Test that slash INSIDE parentheses is not a delimiter
  result_comment <- parse_adduct(
    "[M-C6H10O4 (methylpentose/desoxyhexose-H2O)+H]+"
  )
  expect_false(is_parse_failed(result_comment))

  # Test that slash OUTSIDE parentheses IS a delimiter (alternative adducts)
  result_alternatives <- parse_adduct("[M+H]+/[M+Na]+")
  expect_false(is_parse_failed(result_alternatives))
  # Should parse the first alternative [M+H]+
  expect_equal(unname(result_alternatives["n_mer"]), 1)
  expect_equal(unname(result_alternatives["charge"]), 1)
})
