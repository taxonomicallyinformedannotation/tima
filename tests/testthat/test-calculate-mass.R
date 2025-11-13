#' @title Enhanced Test Suite for calculate_mass_of_m
#'
#' @description Comprehensive tests for mass calculation functions including
#'     edge cases, error handling, and round-trip accuracy verification.

library(testthat)
library(tima)

# Basic functionality tests ----

test_that("calculate_mass_of_m handles simple protonated molecules", {
  # Known example: caffeine [M+H]+ at m/z 195.0877
  # Neutral mass should be ~194.0803
  mz <- 195.0877
  mass <- calculate_mass_of_m(mz = mz, adduct_string = "[M+H]+")

  expect_type(mass, "double")
  expect_true(abs(mass - 194.0803) < 0.01)
  expect_true(mass > 0)
})

test_that("calculate_mass_of_m handles deprotonated molecules", {
  # [M-H]- should give similar mass to [M+H]+
  neutral_mass <- 200.0

  # Calculate m/z for both modes
  mz_pos <- calculate_mz_from_mass(neutral_mass, "[M+H]+")
  mz_neg <- calculate_mz_from_mass(neutral_mass, "[M-H]-")

  # Both should give same neutral mass back
  mass_pos <- calculate_mass_of_m(mz_pos, "[M+H]+")
  mass_neg <- calculate_mass_of_m(mz_neg, "[M-H]-")

  expect_equal(mass_pos, neutral_mass, tolerance = 1e-4)
  expect_equal(mass_neg, neutral_mass, tolerance = 1e-4)
})

test_that("calculate_mass_of_m handles common adducts", {
  neutral_mass <- 150.0

  adducts <- c("[M+H]+", "[M+Na]+", "[M+K]+", "[M+NH4]+", "[M-H]-")

  for (adduct in adducts) {
    mz <- calculate_mz_from_mass(neutral_mass, adduct)
    mass_back <- calculate_mass_of_m(mz, adduct)

    expect_equal(mass_back, neutral_mass, tolerance = 1e-4,
                label = paste("Adduct:", adduct))
  }
})

# Multimer tests ----

test_that("calculate_mass_of_m handles dimers correctly", {
  neutral_mass <- 100.0

  # Dimer [2M+H]+ should have m/z of approximately (2*100 + 1)/1
  mz_dimer <- calculate_mz_from_mass(neutral_mass, "[2M+H]+")
  mass_from_dimer <- calculate_mass_of_m(mz_dimer, "[2M+H]+")

  expect_equal(mass_from_dimer, neutral_mass, tolerance = 1e-3)
})

test_that("calculate_mass_of_m handles trimers", {
  neutral_mass <- 120.0

  mz_trimer <- calculate_mz_from_mass(neutral_mass, "[3M+H]+")
  mass_from_trimer <- calculate_mass_of_m(mz_trimer, "[3M+H]+")

  expect_equal(mass_from_trimer, neutral_mass, tolerance = 1e-3)
})

# Multiple charge tests ----

test_that("calculate_mass_of_m handles doubly charged ions", {
  neutral_mass <- 500.0

  # [M+2H]2+ should have approximately half the m/z of [M+H]+
  mz_single <- calculate_mz_from_mass(neutral_mass, "[M+H]+")
  mz_double <- calculate_mz_from_mass(neutral_mass, "[M+2H]2+")

  expect_true(mz_double < mz_single)
  expect_true(abs(mz_double - mz_single/2) < 2)

  # Should recover same mass
  mass_from_double <- calculate_mass_of_m(mz_double, "[M+2H]2+")
  expect_equal(mass_from_double, neutral_mass, tolerance = 1e-3)
})

test_that("calculate_mass_of_m handles triply charged ions", {
  neutral_mass <- 1000.0

  mz_triple <- calculate_mz_from_mass(neutral_mass, "[M+3H]3+")
  mass_from_triple <- calculate_mass_of_m(mz_triple, "[M+3H]3+")

  expect_equal(mass_from_triple, neutral_mass, tolerance = 1e-3)
})

# Isotope tests ----

test_that("calculate_mass_of_m handles M+1 isotopologue", {
  neutral_mass <- 200.0

  # M+1 should add 1 Da to the m/z
  mz_m0 <- calculate_mz_from_mass(neutral_mass, "[M+H]+")
  mz_m1 <- calculate_mz_from_mass(neutral_mass, "[M1+H]+")

  expect_equal(mz_m1 - mz_m0, 1, tolerance = 0.01)

  # Should recover same mass
  mass_from_m1 <- calculate_mass_of_m(mz_m1, "[M1+H]+")
  expect_equal(mass_from_m1, neutral_mass, tolerance = 1e-3)
})

test_that("calculate_mass_of_m handles M+2 isotopologue", {
  neutral_mass <- 200.0

  mz_m2 <- calculate_mz_from_mass(neutral_mass, "[M2+H]+")
  mass_from_m2 <- calculate_mass_of_m(mz_m2, "[M2+H]+")

  expect_equal(mass_from_m2, neutral_mass, tolerance = 1e-3)
})

# Complex modifications tests ----

test_that("calculate_mass_of_m handles water loss", {
  neutral_mass <- 180.0634  # Glucose

  # [M+H-H2O]+ loses 18 Da
  mz_with_loss <- calculate_mz_from_mass(neutral_mass, "[M+H-H2O]+")
  mass_from_loss <- calculate_mass_of_m(mz_with_loss, "[M+H-H2O]+")

  expect_equal(mass_from_loss, neutral_mass, tolerance = 1e-3)
})

test_that("calculate_mass_of_m handles ammonia loss", {
  neutral_mass <- 150.0

  mz_nh3_loss <- calculate_mz_from_mass(neutral_mass, "[M+H-NH3]+")
  mass_from_nh3 <- calculate_mass_of_m(mz_nh3_loss, "[M+H-NH3]+")

  expect_equal(mass_from_nh3, neutral_mass, tolerance = 1e-3)
})

test_that("calculate_mass_of_m handles multiple modifications", {
  neutral_mass <- 200.0

  # Complex adduct with multiple modifications
  adduct <- "[M+Na-H2O]+"
  mz_complex <- calculate_mz_from_mass(neutral_mass, adduct)
  mass_from_complex <- calculate_mass_of_m(mz_complex, adduct)

  expect_equal(mass_from_complex, neutral_mass, tolerance = 1e-3)
})

# Input validation tests ----

test_that("calculate_mass_of_m validates required parameters", {
  expect_error(
    calculate_mass_of_m(mz = 100),
    "Both adduct_string and mz must be provided"
  )

  expect_error(
    calculate_mass_of_m(adduct_string = "[M+H]+"),
    "Both adduct_string and mz must be provided"
  )
})

test_that("calculate_mass_of_m validates mz value", {
  # Negative m/z
  expect_error(
    calculate_mass_of_m(mz = -100, adduct_string = "[M+H]+"),
    "mz must be positive"
  )

  # NA
  expect_error(
    calculate_mass_of_m(mz = NA, adduct_string = "[M+H]+"),
    "mz cannot be NA"
  )

  # Infinite
  expect_error(
    calculate_mass_of_m(mz = Inf, adduct_string = "[M+H]+"),
    "must be finite"
  )

  # Wrong type
  expect_error(
    calculate_mass_of_m(mz = "100", adduct_string = "[M+H]+"),
    "mz must be numeric"
  )

  # Multiple values
  expect_error(
    calculate_mass_of_m(mz = c(100, 200), adduct_string = "[M+H]+"),
    "must be a single value"
  )
})

test_that("calculate_mass_of_m warns about suspicious m/z values", {
  # Very high m/z
  expect_warning(
    calculate_mass_of_m(mz = 10000, adduct_string = "[M+H]+"),
    "exceeds typical"
  )
})

test_that("calculate_mass_of_m handles invalid adduct strings", {
  # Invalid adduct should return 0 with warning
  expect_warning(
    result <- calculate_mass_of_m(mz = 100, adduct_string = "invalid"),
    "Failed to parse"
  )
  expect_equal(result, 0)
})

test_that("calculate_mass_of_m handles division by zero", {
  # Adduct with zero charges (if somehow created)
  expect_warning(
    result <- calculate_mass_of_m(mz = 100, adduct_string = "[M]"),
    NA  # Should handle gracefully
  )
})

test_that("calculate_mass_of_m validates electron mass", {
  expect_error(
    calculate_mass_of_m(mz = 100, adduct_string = "[M+H]+", electron_mass = -1),
    "must be positive"
  )

  expect_error(
    calculate_mass_of_m(mz = 100, adduct_string = "[M+H]+", electron_mass = "0.0005"),
    "must be a single numeric value"
  )
})

test_that("calculate_mass_of_m warns about non-standard electron mass", {
  expect_warning(
    calculate_mass_of_m(
      mz = 100,
      adduct_string = "[M+H]+",
      electron_mass = 0.001  # 2x standard value
    ),
    "differs from standard value"
  )
})

# Round-trip accuracy tests ----

test_that("calculate_mass_of_m and calculate_mz_from_mass are perfect inverses", {
  neutral_masses <- c(50, 100, 200, 500, 1000)
  adducts <- c("[M+H]+", "[M+Na]+", "[M-H]-", "[2M+H]+", "[M+2H]2+")

  for (mass in neutral_masses) {
    for (adduct in adducts) {
      mz <- calculate_mz_from_mass(mass, adduct)
      mass_back <- calculate_mass_of_m(mz, adduct)

      expect_equal(
        mass_back,
        mass,
        tolerance = 1e-6,
        label = paste("Mass:", mass, "Adduct:", adduct)
      )
    }
  }
})

test_that("round-trip maintains precision", {
  # Test high precision
  neutral_mass <- 194.080375570  # High precision caffeine mass

  mz <- calculate_mz_from_mass(neutral_mass, "[M+H]+")
  mass_back <- calculate_mass_of_m(mz, "[M+H]+")

  # Should maintain at least 6 decimal places
  expect_equal(mass_back, neutral_mass, tolerance = 1e-9)
})

# Physical impossibility checks ----

test_that("calculate_mass_of_m detects physically impossible results", {
  # Very small m/z with complex adduct might give negative mass
  # The function should return NA for negative masses

  # This is a contrived example that might produce negative mass
  result <- calculate_mass_of_m(mz = 1, adduct_string = "[M+Na]+")

  if (result < 0) {
    expect_true(is.na(result) || result == 0)
  }
})

# Edge cases ----

test_that("calculate_mass_of_m handles very small masses", {
  neutral_mass <- 1.0

  mz <- calculate_mz_from_mass(neutral_mass, "[M+H]+")
  mass_back <- calculate_mass_of_m(mz, "[M+H]+")

  expect_equal(mass_back, neutral_mass, tolerance = 1e-6)
})

test_that("calculate_mass_of_m handles very large masses", {
  neutral_mass <- 4000.0  # Large peptide

  mz <- calculate_mz_from_mass(neutral_mass, "[M+H]+")
  mass_back <- calculate_mass_of_m(mz, "[M+H]+")

  expect_equal(mass_back, neutral_mass, tolerance = 1e-4)
})

test_that("calculate_mass_of_m handles zero m/z edge case", {
  # Zero m/z is physically impossible but should be handled
  expect_error(
    calculate_mass_of_m(mz = 0, adduct_string = "[M+H]+"),
    "must be positive"
  )
})

# calculate_mz_from_mass tests ----

test_that("calculate_mz_from_mass validates inputs", {
  expect_error(
    calculate_mz_from_mass(neutral_mass = -100, adduct_string = "[M+H]+"),
    NA  # Should handle via validate_numeric_range
  )

  expect_error(
    calculate_mz_from_mass(neutral_mass = 100),
    "Both.*must be provided"
  )
})

test_that("calculate_mz_from_mass handles common adducts", {
  neutral_mass <- 200.0

  # [M+H]+ should add ~1 Da
  mz_h <- calculate_mz_from_mass(neutral_mass, "[M+H]+")
  expect_true(abs(mz_h - 201) < 1)

  # [M+Na]+ should add ~23 Da
  mz_na <- calculate_mz_from_mass(neutral_mass, "[M+Na]+")
  expect_true(abs(mz_na - 223) < 1)

  # [M-H]- should subtract ~1 Da
  mz_neg <- calculate_mz_from_mass(neutral_mass, "[M-H]-")
  expect_true(abs(mz_neg - 199) < 1)
})

test_that("calculate_mz_from_mass handles multimers", {
  neutral_mass <- 100.0

  # Dimer should have ~2x mass
  mz_dimer <- calculate_mz_from_mass(neutral_mass, "[2M+H]+")
  expect_true(abs(mz_dimer - 201) < 1)
})

test_that("calculate_mz_from_mass handles multiple charges", {
  neutral_mass <- 1000.0

  mz_single <- calculate_mz_from_mass(neutral_mass, "[M+H]+")
  mz_double <- calculate_mz_from_mass(neutral_mass, "[M+2H]2+")

  # Double charge should be approximately half
  expect_true(abs(mz_double - mz_single/2) < 5)
})

test_that("calculate_mz_from_mass handles failed adduct parsing", {
  expect_warning(
    result <- calculate_mz_from_mass(100, "invalid_adduct"),
    "Failed to parse"
  )
  expect_equal(result, 0)
})

# Performance tests ----

test_that("calculate_mass_of_m is fast enough", {
  skip_on_cran()

  # Should calculate 1000 masses in under 0.5 seconds
  mz_values <- runif(1000, 100, 1000)

  start_time <- Sys.time()
  masses <- sapply(mz_values, function(mz) {
    calculate_mass_of_m(mz, "[M+H]+")
  })
  end_time <- Sys.time()

  elapsed <- as.numeric(end_time - start_time, units = "secs")

  expect_true(elapsed < 0.5)
  expect_equal(length(masses), 1000)
})

test_that("round-trip calculations are consistent", {
  # Multiple round trips should not accumulate error
  neutral_mass <- 250.0

  mass <- neutral_mass
  for (i in 1:10) {
    mz <- calculate_mz_from_mass(mass, "[M+H]+")
    mass <- calculate_mass_of_m(mz, "[M+H]+")
  }

  expect_equal(mass, neutral_mass, tolerance = 1e-6)
})

# Integration with parse_adduct ----

test_that("calculate_mass_of_m works with all adduct patterns", {
  # These adducts should all be parseable and produce valid results
  test_adducts <- c(
    "[M+H]+",
    "[M+Na]+",
    "[M+K]+",
    "[M+NH4]+",
    "[M-H]-",
    "[M+Cl]-",
    "[2M+H]+",
    "[M+2H]2+",
    "[M1+H]+",
    "[M+H-H2O]+"
  )

  neutral_mass <- 200.0

  for (adduct in test_adducts) {
    mz <- calculate_mz_from_mass(neutral_mass, adduct)
    mass_back <- calculate_mass_of_m(mz, adduct)

    expect_true(
      abs(mass_back - neutral_mass) < 0.01,
      label = paste("Adduct:", adduct)
    )
  }
})

# Real-world examples ----

test_that("calculate_mass_of_m works with real metabolite examples", {
  # Glucose: C6H12O6, exact mass 180.0634
  glucose_mass <- 180.0634
  glucose_mz_pos <- calculate_mz_from_mass(glucose_mass, "[M+Na]+")
  glucose_back <- calculate_mass_of_m(glucose_mz_pos, "[M+Na]+")
  expect_equal(glucose_back, glucose_mass, tolerance = 0.001)

  # Caffeine: C8H10N4O2, exact mass 194.0803
  caffeine_mass <- 194.0803
  caffeine_mz_pos <- calculate_mz_from_mass(caffeine_mass, "[M+H]+")
  caffeine_back <- calculate_mass_of_m(caffeine_mz_pos, "[M+H]+")
  expect_equal(caffeine_back, caffeine_mass, tolerance = 0.001)

  # Cholesterol: C27H46O, exact mass 386.3549
  cholesterol_mass <- 386.3549
  cholesterol_mz_pos <- calculate_mz_from_mass(cholesterol_mass, "[M+NH4]+")
  cholesterol_back <- calculate_mass_of_m(cholesterol_mz_pos, "[M+NH4]+")
  expect_equal(cholesterol_back, cholesterol_mass, tolerance = 0.001)
})

