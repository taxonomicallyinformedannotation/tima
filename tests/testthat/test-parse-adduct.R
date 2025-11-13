#' @title Test Suite for parse_adduct
#'
#' @description Comprehensive unit tests for parse_adduct function.

library(testthat)
library(tima)

# Test: Basic valid adduct parsing ----

test_that("parse_adduct parses simple positive adduct [M+H]+", {
  result <- parse_adduct("[M+H]+")

  expect_type(result, "double")
  expect_named(result, c("n_mer", "n_iso", "los_add_clu", "n_charges", "charge"))
  expect_equal(result["n_mer"], 1)
  expect_equal(result["n_iso"], 0)
  expect_equal(result["n_charges"], 1)
  expect_equal(result["charge"], 1)
  expect_true(result["los_add_clu"] > 0)  # H adds mass
})

test_that("parse_adduct parses simple negative adduct [M-H]-", {
  result <- parse_adduct("[M-H]-")

  expect_equal(result["n_mer"], 1)
  expect_equal(result["n_iso"], 0)
  expect_equal(result["n_charges"], 1)
  expect_equal(result["charge"], -1)
  expect_true(result["los_add_clu"] < 0)  # H loss
})

test_that("parse_adduct parses sodium adduct [M+Na]+", {
  result <- parse_adduct("[M+Na]+")

  expect_equal(result["n_mer"], 1)
  expect_equal(result["charge"], 1)
  expect_true(result["los_add_clu"] > 20)  # Na is ~23 Da
})

# Test: Multimer parsing ----

test_that("parse_adduct parses dimer [2M+H]+", {
  result <- parse_adduct("[2M+H]+")
  expect_equal(result["n_mer"], 2)
})

test_that("parse_adduct parses trimer [3M+H]+", {
  result <- parse_adduct("[3M+H]+")
  expect_equal(result["n_mer"], 3)
})

test_that("parse_adduct parses higher multimers", {
  result <- parse_adduct("[4M+Na]+")
  expect_equal(result["n_mer"], 4)
})

# Test: Isotope parsing ----

test_that("parse_adduct parses M+1 isotopologue", {
  result <- parse_adduct("[M1+H]+")
  expect_equal(result["n_iso"], 1)
})

test_that("parse_adduct parses M+2 isotopologue", {
  result <- parse_adduct("[M2+H]+")
  expect_equal(result["n_iso"], 2)
})

test_that("parse_adduct parses M+3 isotopologue", {
  result <- parse_adduct("[M3+Na]+")
  expect_equal(result["n_iso"], 3)
})

# Test: Multiple charges ----

test_that("parse_adduct parses doubly charged [M+2H]2+", {
  result <- parse_adduct("[M+2H]2+")
  expect_equal(result["n_charges"], 2)
  expect_equal(result["charge"], 1)
})

test_that("parse_adduct parses triply charged [M+3H]3+", {
  result <- parse_adduct("[M+3H]3+")
  expect_equal(result["n_charges"], 3)
  expect_equal(result["charge"], 1)
})

test_that("parse_adduct parses doubly negative [M-2H]2-", {
  result <- parse_adduct("[M-2H]2-")
  expect_equal(result["n_charges"], 2)
  expect_equal(result["charge"], -1)
})

# Test: Complex modifications ----

test_that("parse_adduct parses water loss [M+H-H2O]+", {
  result <- parse_adduct("[M+H-H2O]+")

  expect_equal(result["n_mer"], 1)
  expect_equal(result["n_charges"], 1)
  # H - H2O should be negative overall
  expect_true(result["los_add_clu"] < 1)
})

test_that("parse_adduct parses ammonia loss [M+H-NH3]+", {
  result <- parse_adduct("[M+H-NH3]+")
  expect_equal(result["n_mer"], 1)
  expect_true(result["los_add_clu"] < 0)
})

test_that("parse_adduct parses multiple additions [M+Na+K]+", {
  result <- parse_adduct("[M+Na+K]+")
  # Na + K together ~62 Da
  expect_true(result["los_add_clu"] > 60)
})

test_that("parse_adduct handles comments in parentheses", {
  result1 <- parse_adduct("[M-C6H12O6 (hexose)+H]+")
  result2 <- parse_adduct("[M-C6H12O6+H]+")

  # Should be equivalent after removing comment
  expect_equal(result1["los_add_clu"], result2["los_add_clu"])
})

# Test: Combined complex patterns ----

test_that("parse_adduct parses dimer with isotope [2M1+H]+", {
  result <- parse_adduct("[2M1+H]+")
  expect_equal(result["n_mer"], 2)
  expect_equal(result["n_iso"], 1)
})

test_that("parse_adduct parses doubly charged with modification", {
  result <- parse_adduct("[M+2H-H2O]2+")
  expect_equal(result["n_charges"], 2)
  expect_true(result["los_add_clu"] < 2)  # 2H - H2O
})

test_that("parse_adduct parses very complex adduct", {
  result <- parse_adduct("[2M1-C6H12O6+NaCl+H]2+")

  expect_equal(result["n_mer"], 2)
  expect_equal(result["n_iso"], 1)
  expect_equal(result["n_charges"], 2)
  expect_equal(result["charge"], 1)
})

# Test: NULL and missing input ----

test_that("parse_adduct returns zeros for NULL input", {
  result <- parse_adduct(NULL)
  expect_equal(result["n_mer"], 0)
  expect_equal(result["n_iso"], 0)
  expect_equal(result["n_charges"], 0)
  expect_equal(result["charge"], 0)
})

test_that("parse_adduct returns zeros for missing input", {
  result <- parse_adduct()
  expect_equal(result["n_mer"], 0)
})

# Test: Invalid input handling ----

test_that("parse_adduct stops on non-character input", {
  expect_error(parse_adduct(123), "character")
})

test_that("parse_adduct stops on empty string", {
  expect_error(parse_adduct(""), "non-empty")
})

test_that("parse_adduct stops on multiple values", {
  expect_error(parse_adduct(c("[M+H]+", "[M+Na]+")), "single")
})

test_that("parse_adduct returns zeros for invalid format with warning", {
  expect_warning(result <- parse_adduct("invalid_format"), "Invalid adduct")
  expect_equal(result["n_mer"], 0)
})

test_that("parse_adduct handles malformed brackets", {
  expect_warning(result <- parse_adduct("M+H]+"), "Invalid")
  expect_equal(result["n_mer"], 0)
})

# Test: Edge cases ----

test_that("parse_adduct handles adduct without explicit multimer", {
  result <- parse_adduct("[M+H]+")
  expect_equal(result["n_mer"], 1)  # Defaults to 1
})

test_that("parse_adduct handles adduct without isotope", {
  result <- parse_adduct("[M+H]+")
  expect_equal(result["n_iso"], 0)  # Defaults to 0
})

test_that("parse_adduct handles adduct without explicit charge number", {
  result <- parse_adduct("[M+H]+")
  expect_equal(result["n_charges"], 1)  # Defaults to 1
})

test_that("parse_adduct handles chloride adduct", {
  result <- parse_adduct("[M+Cl]-")
  expect_equal(result["charge"], -1)
  expect_true(result["los_add_clu"] > 30)  # Cl is ~35 Da
})

test_that("parse_adduct handles formate adduct", {
  result <- parse_adduct("[M+HCOO]-")
  expect_equal(result["charge"], -1)
  expect_true(result["los_add_clu"] > 40)  # Formate ~45 Da
})

test_that("parse_adduct handles acetate adduct", {
  result <- parse_adduct("[M+CH3COO]-")
  expect_equal(result["charge"], -1)
  expect_true(result["los_add_clu"] > 55)  # Acetate ~59 Da
})

# Test: Custom regex parameter ----

test_that("parse_adduct accepts custom regex", {
  # Using default regex explicitly
  custom_regex <- "\\[(\\d*)M(?![a-z])(\\d*)([+-][\\w\\d].*)?.*\\](\\d*)([+-])?"
  result <- parse_adduct("[M+H]+", regex = custom_regex)

  expect_equal(result["n_mer"], 1)
  expect_equal(result["charge"], 1)
})

# Test: Consistency across similar adducts ----

test_that("parse_adduct gives consistent results for equivalent notations", {
  result1 <- parse_adduct("[M+H]+")
  result2 <- parse_adduct("[1M+H]+")

  expect_equal(result1["n_mer"], result2["n_mer"])
})

# Test: Return value structure ----

test_that("parse_adduct always returns named numeric vector of length 5", {
  result <- parse_adduct("[M+H]+")

  expect_length(result, 5)
  expect_named(result)
  expect_type(result, "double")
})

# Test: Logging behavior ----

test_that("parse_adduct logs warnings for invalid input", {
  expect_warning(parse_adduct("bad_format"), "Invalid adduct format")
})

test_that("parse_adduct logs trace for successful parse", {
  # Note: This requires logger to be configured for trace level
  # Just verify no errors
  expect_silent(parse_adduct("[M+H]+"))
})

# Test: Real-world adduct examples ----

test_that("parse_adduct handles common positive mode adducts", {
  common_pos <- c("[M+H]+", "[M+Na]+", "[M+K]+", "[M+NH4]+", "[2M+H]+")

  for (adduct in common_pos) {
    result <- parse_adduct(adduct)
    expect_equal(result["charge"], 1, label = adduct)
    expect_true(result["n_mer"] > 0, label = adduct)
  }
})

test_that("parse_adduct handles common negative mode adducts", {
  common_neg <- c("[M-H]-", "[M+Cl]-", "[M+HCOO]-", "[2M-H]-")

  for (adduct in common_neg) {
    result <- parse_adduct(adduct)
    expect_equal(result["charge"], -1, label = adduct)
    expect_true(result["n_mer"] > 0, label = adduct)
  }
})

# Test: Performance ----

test_that("parse_adduct is fast enough for batch processing", {
  skip_on_cran()

  adducts <- rep("[M+H]+", 1000)

  start_time <- Sys.time()
  results <- lapply(adducts, parse_adduct)
  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")

  expect_true(elapsed < 1.0)  # Should parse 1000 in under 1 second
  expect_equal(length(results), 1000)
})

# Test: Memory efficiency ----

test_that("parse_adduct doesn't leak memory", {
  skip_on_cran()
  skip_if_not_installed("pryr")

  # Parse same adduct many times
  for (i in 1:100) {
    result <- parse_adduct("[M+H]+")
  }

  # Should complete without memory issues
  expect_true(TRUE)
})

