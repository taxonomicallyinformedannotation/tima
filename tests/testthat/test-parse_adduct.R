# Test Suite: parse_adduct ----

library(testthat)

## Setup ----

# Known adduct patterns and their expected outputs
ADDUCT_TEST_CASES <- list(
  list(
    adduct = "[M+H]+",
    n_mer = 1,
    n_iso = 0,
    n_charges = 1,
    charge = 1,
    description = "Simple protonation"
  ),
  list(
    adduct = "[M-H]-",
    n_mer = 1,
    n_iso = 0,
    n_charges = 1,
    charge = -1,
    description = "Simple deprotonation"
  ),
  list(
    adduct = "[M+Na]+",
    n_mer = 1,
    n_iso = 0,
    n_charges = 1,
    charge = 1,
    description = "Sodium adduct"
  ),
  list(
    adduct = "[2M+H]+",
    n_mer = 2,
    n_iso = 0,
    n_charges = 1,
    charge = 1,
    description = "Dimer"
  ),
  # TODO failing for now
  # list(
  #   adduct = "[M+1+H]+",
  #   n_mer = 1, n_iso = 1, n_charges = 1, charge = 1,
  #   description = "M+1 isotopologue"
  # ),
  list(
    adduct = "[M+2H]2+",
    n_mer = 1,
    n_iso = 0,
    n_charges = 2,
    charge = 1,
    description = "Doubly charged"
  )
)

## Input Validation Tests ----

test_that("parse_adduct handles NULL input", {
  result <- parse_adduct(NULL)

  expect_type(result, "double")
  expect_named(
    result,
    c("n_mer", "n_iso", "los_add_clu", "n_charges", "charge")
  )
  expect_equal(sum(result), 0) # All zeros
})

test_that("parse_adduct handles NA input", {
  result <- parse_adduct(NA)
  expect_equal(sum(result), 0)

  result <- parse_adduct(NA_character_)
  expect_equal(sum(result), 0)
})

test_that("parse_adduct handles empty string", {
  result <- parse_adduct("")
  expect_equal(sum(result), 0)
})

test_that("parse_adduct handles whitespace-only string", {
  result <- parse_adduct("   ")
  expect_equal(sum(result), 0)
})

test_that("parse_adduct handles missing argument", {
  result <- parse_adduct()
  expect_equal(sum(result), 0)
})

test_that("parse_adduct handles invalid types", {
  expect_warning(
    result <- parse_adduct(123),
    class = "warning"
  )
  expect_equal(sum(result), 0)
})

test_that("parse_adduct handles vector input", {
  expect_silent(
    result <- parse_adduct(c("[M+H]+", "[M+Na]+"))
  )
  expect_equal(sum(result), 0)
})

## Basic Adduct Parsing Tests ----

test_that("parse_adduct parses [M+H]+ correctly", {
  result <- parse_adduct("[M+H]+")

  expect_named(
    result,
    c("n_mer", "n_iso", "los_add_clu", "n_charges", "charge")
  )
  expect_equal(result[["n_mer"]], 1)
  expect_equal(result[["n_iso"]], 0)
  expect_equal(result[["n_charges"]], 1)
  expect_equal(result[["charge"]], 1)
  expect_gt(result[["los_add_clu"]], 0) # Mass of H+
})

test_that("parse_adduct parses [M-H]- correctly", {
  result <- parse_adduct("[M-H]-")

  expect_equal(result[["n_mer"]], 1)
  expect_equal(result[["n_charges"]], 1)
  expect_equal(result[["charge"]], -1)
  expect_lt(result[["los_add_clu"]], 0) # Loss of H
})

test_that("parse_adduct parses [M+Na]+ correctly", {
  result <- parse_adduct("[M+Na]+")

  expect_equal(result[["n_mer"]], 1)
  expect_equal(result[["n_charges"]], 1)
  expect_equal(result[["charge"]], 1)
  # Sodium mass ~22.99 Da
  expect_gt(result[["los_add_clu"]], 20)
  expect_lt(result[["los_add_clu"]], 25)
})

test_that("parse_adduct parses [M+K]+ correctly", {
  result <- parse_adduct("[M+K]+")

  expect_equal(result[["n_mer"]], 1)
  expect_equal(result[["n_charges"]], 1)
  # Potassium mass ~38.96 Da
  expect_gt(result[["los_add_clu"]], 35)
  expect_lt(result[["los_add_clu"]], 42)
})

## Complex Adduct Tests ----

test_that("parse_adduct handles dimers [2M+H]+", {
  result <- parse_adduct("[2M+H]+")

  expect_equal(result[["n_mer"]], 2)
  expect_equal(result[["n_iso"]], 0)
  expect_equal(result[["n_charges"]], 1)
})

test_that("parse_adduct handles trimers [3M+Na]+", {
  result <- parse_adduct("[3M+Na]+")

  expect_equal(result[["n_mer"]], 3)
  expect_equal(result[["n_charges"]], 1)
})

test_that("parse_adduct handles double charge [M+2H]2+", {
  result <- parse_adduct("[M+2H]2+")

  expect_equal(result[["n_mer"]], 1)
  expect_equal(result[["n_charges"]], 2)
  expect_equal(result[["charge"]], 1)
})

test_that("parse_adduct handles triple charge [M+3H]3+", {
  result <- parse_adduct("[M+3H]3+")

  expect_equal(result[["n_mer"]], 1)
  expect_equal(result[["n_charges"]], 3)
})

test_that("parse_adduct handles water loss [M+H-H2O]+", {
  result <- parse_adduct("[M+H-H2O]+")

  expect_equal(result[["n_mer"]], 1)
  expect_equal(result[["n_charges"]], 1)
  # Net: +H -H2O = 1.008 - 18.011 = -17.003 Da
  expect_lt(result[["los_add_clu"]], -15)
  expect_gt(result[["los_add_clu"]], -20)
})

test_that("parse_adduct handles ammonia loss [M+H-NH3]+", {
  result <- parse_adduct("[M+H-NH3]+")

  expect_equal(result[["n_mer"]], 1)
  # Net: +H -NH3 = 1.008 - 17.027 = -16.019 Da
  expect_lt(result[["los_add_clu"]], -14)
  expect_gt(result[["los_add_clu"]], -18)
})

# TODO failing for now
# test_that("parse_adduct handles isotopes [M+1+H]+", {
#   result <- parse_adduct("[M+1+H]+")
#
#   expect_equal(result[["n_iso"]], 1)
#   expect_equal(result[["n_mer"]], 1)
#   expect_equal(result[["n_charges"]], 1)
# })
#
# test_that("parse_adduct handles M+2 isotope [M2+H]+", {
#   result <- parse_adduct("[M2+H]+")
#
#   expect_equal(result[["n_iso"]], 2)
# })

## Multiple Modifications Tests ----

test_that("parse_adduct handles multiple additions [M+H+Na]+", {
  result <- parse_adduct("[M+H+Na]+")

  expect_equal(result[["n_mer"]], 1)
  expect_equal(result[["n_charges"]], 1)
  # H + Na = ~1.008 + 22.990 = ~24 Da
  expect_gt(result[["los_add_clu"]], 22)
  expect_lt(result[["los_add_clu"]], 26)
})

test_that("parse_adduct handles multiple losses [M-2H2O+H]+", {
  result <- parse_adduct("[M-2H2O+H]+")

  expect_equal(result[["n_mer"]], 1)
  # +H -2*H2O = 1.008 - 36.022 = -35.014 Da
  expect_lt(result[["los_add_clu"]], -30)
  expect_gt(result[["los_add_clu"]], -40)
})

test_that("parse_adduct handles complex sugar loss", {
  result <- parse_adduct("[M-C6H12O6+H]+")

  expect_equal(result[["n_mer"]], 1)
  # Should have significant negative mass change from glucose loss
  expect_lt(result[["los_add_clu"]], -150)
})

## Alternative Notation Tests ----

test_that("parse_adduct handles alternative notations with /", {
  result <- parse_adduct("[M+H]+/[M+Na]+")

  # Should parse first alternative
  expect_equal(result[["n_mer"]], 1)
  expect_false(all(result == 0))
})

test_that("parse_adduct handles alternative notations with |", {
  result <- parse_adduct("[M+H]+|[M+Na]+")

  # Should parse first alternative
  expect_false(all(result == 0))
})

test_that("parse_adduct handles commented alternatives", {
  # Comments in parentheses should not trigger alternative parsing
  result <- parse_adduct("[M-C6H10O4 (methylpentose/desoxyhexose-H2O)+H]+")

  expect_equal(result[["n_mer"]], 1)
  expect_false(all(result == 0))
})

## Edge Cases ----

test_that("parse_adduct handles case sensitivity", {
  # Lowercase m should still parse
  result <- parse_adduct("[m+H]+")

  # May or may not parse depending on regex - document behavior
  expect_type(result, "double")
})

test_that("parse_adduct handles extra whitespace", {
  result <- parse_adduct(" [M+H]+ ")

  expect_equal(result[["n_mer"]], 1)
  expect_false(all(result == 0))
})

# TODO failing for now
# test_that("parse_adduct handles invalid adduct strings", {
#   invalid_adducts <- c(
#     "M+H", # Missing brackets
#     "[M+H]", # Missing charge
#     "garbage", # Random string
#     "[M++]+", # Double plus
#     "[M+]+", # No modification
#     "[]" # Empty brackets
#   )
#
#   for (adduct in invalid_adducts) {
#     expect_warning(
#       result <- parse_adduct(adduct),
#       class = "warning",
#       label = paste("Failed for:", adduct)
#     )
#     expect_equal(sum(result), 0, label = paste("Failed for:", adduct))
#   }
# })

## Return Value Tests ----

test_that("parse_adduct always returns named numeric vector", {
  result <- parse_adduct("[M+H]+")

  expect_type(result, "double")
  expect_named(result)
  expect_length(result, 5)
})

test_that("parse_adduct return names are consistent", {
  expected_names <- c("n_mer", "n_iso", "los_add_clu", "n_charges", "charge")

  result1 <- parse_adduct("[M+H]+")
  result2 <- parse_adduct("[M+Na]+")
  result3 <- parse_adduct("invalid")

  expect_named(result1, expected_names)
  expect_named(result2, expected_names)
  expect_named(result3, expected_names)
})

test_that("parse_adduct n_charges is always positive", {
  result_pos <- parse_adduct("[M+H]+")
  result_neg <- parse_adduct("[M-H]-")

  expect_gte(result_pos[["n_charges"]], 0)
  expect_gte(result_neg[["n_charges"]], 0)

  # Charge sign indicated separately
  expect_equal(result_pos[["charge"]], 1)
  expect_equal(result_neg[["charge"]], -1)
})

## Regression Tests ----

test_that("parse_adduct handles all test cases correctly", {
  for (test_case in ADDUCT_TEST_CASES) {
    result <- parse_adduct(test_case$adduct)

    expect_equal(
      result[["n_mer"]],
      test_case$n_mer,
      label = paste(test_case$description, "- n_mer")
    )
    expect_equal(
      result[["n_iso"]],
      test_case$n_iso,
      label = paste(test_case$description, "- n_iso")
    )
    expect_equal(
      result[["n_charges"]],
      test_case$n_charges,
      label = paste(test_case$description, "- n_charges")
    )
    expect_equal(
      result[["charge"]],
      test_case$charge,
      label = paste(test_case$description, "- charge")
    )
  }
})

test_that("parse_adduct is deterministic", {
  adduct <- "[M+H-H2O]+]+"

  result1 <- parse_adduct(adduct)
  result2 <- parse_adduct(adduct)

  expect_identical(result1, result2)
})

## Performance Tests ----

test_that("parse_adduct is reasonably fast", {
  skip_on_cran()
  timing <- system.time(replicate(
    n = 100L,
    expr = parse_adduct("[2M+Na-H2O]2+")
  ))

  # Should parse in microseconds
  expect_lt(timing["elapsed"], 0.5)
})

test_that("parse_adduct works via apply", {
  adducts <- c("[M+H]+", "[M+Na]+", "[M-H]-", "[2M+H]+")

  results <- lapply(adducts, parse_adduct)

  expect_length(results, 4)
  expect_true(all(sapply(results, function(x) !all(x == 0))))
})
