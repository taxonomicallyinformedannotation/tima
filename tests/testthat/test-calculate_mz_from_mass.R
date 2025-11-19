# Test Suite: calculate_mz_from_mass ----

library(testthat)

## Basic mz calculation ----

test_that("calculate_mz_from_mass calculates mz for [M+H]+", {
  neutral_mass <- 200.0
  mz <- calculate_mz_from_mass(neutral_mass, "[M+H]+")

  expect_type(mz, "double")
  expect_true(mz > neutral_mass)
  expect_true(abs(mz - 201) < 1)
})

test_that("calculate_mz_from_mass calculates mz for [M+Na]+", {
  neutral_mass <- 200.0
  mz <- calculate_mz_from_mass(neutral_mass, "[M+Na]+")

  expect_true(abs(mz - 223) < 1) # Na adds ~23 Da
})

test_that("calculate_mz_from_mass calculates mz for [M-H]-", {
  neutral_mass <- 200.0
  mz <- calculate_mz_from_mass(neutral_mass, "[M-H]-")

  expect_true(abs(mz - 199) < 1) # H subtracts ~1 Da
})

## Parameter validation ----

test_that("calculate_mz_from_mass requires neutral_mass", {
  expect_error(
    calculate_mz_from_mass(adduct_string = "[M+H]+"),
    "must be provided"
  )
})

test_that("calculate_mz_from_mass requires adduct_string", {
  expect_error(
    calculate_mz_from_mass(neutral_mass = 100),
    "must be provided"
  )
})

test_that("calculate_mz_from_mass validates neutral_mass range", {
  # Should accept positive values without error
  expect_silent(calculate_mz_from_mass(100, "[M+H]+"))

  # Should accept small positive values
  expect_silent(calculate_mz_from_mass(0.1, "[M+H]+"))

  # Very large values should still work
  expect_silent(calculate_mz_from_mass(10000, "[M+H]+"))
})

## Invalid adduct handling ----

test_that("calculate_mz_from_mass handles invalid adduct gracefully", {
  # Invalid adducts should return 0 with a warning
  expect_warning(
    mz <- calculate_mz_from_mass(100, "invalid_adduct"),
    "Failed to parse"
  )
  expect_equal(mz, 0)
})

test_that("calculate_mz_from_mass handles empty adduct", {
  expect_warning(
    mz <- calculate_mz_from_mass(100, ""),
    "Failed to parse"
  )
  expect_equal(mz, 0)
})

## Multimers ----

test_that("calculate_mz_from_mass handles dimer", {
  neutral_mass <- 100.0
  mz <- calculate_mz_from_mass(neutral_mass, "[2M+H]+")

  # Should be approximately 2*mass + H
  expect_true(abs(mz - 201) < 1)
})

test_that("calculate_mz_from_mass handles trimer", {
  neutral_mass <- 100.0
  mz <- calculate_mz_from_mass(neutral_mass, "[3M+H]+")

  expect_true(abs(mz - 301) < 1)
})

## Multiple charges ----

test_that("calculate_mz_from_mass handles doubly charged", {
  neutral_mass <- 1000.0

  mz_single <- calculate_mz_from_mass(neutral_mass, "[M+H]+")
  mz_double <- calculate_mz_from_mass(neutral_mass, "[M+2H]2+")

  # Double charge should be approximately half
  expect_true(mz_double < mz_single)
  expect_true(abs(mz_double - mz_single / 2) < 5)
})

test_that("calculate_mz_from_mass handles triply charged", {
  neutral_mass <- 1500.0

  mz_triple <- calculate_mz_from_mass(neutral_mass, "[M+3H]3+")

  # Should be approximately mass/3
  expect_true(mz_triple < neutral_mass)
})


## Round-trip accuracy ----

test_that("mass -> mz -> mass round-trip is accurate for [M+H]+", {
  neutral_mass <- 250.0

  mz <- calculate_mz_from_mass(neutral_mass, "[M+H]+")
  mass_back <- calculate_mass_of_m(mz = mz, adduct_string = "[M+H]+")

  expect_equal(mass_back, neutral_mass, tolerance = 1e-6)
})

test_that("mass -> mz -> mass round-trip works for [M+Na]+", {
  neutral_mass <- 180.0634

  mz <- calculate_mz_from_mass(neutral_mass, "[M+Na]+")
  mass_back <- calculate_mass_of_m(mz = mz, adduct_string = "[M+Na]+")

  expect_equal(mass_back, neutral_mass, tolerance = 1e-6)
})

test_that("mass -> mz -> mass round-trip works for [M-H]-", {
  neutral_mass <- 194.0803

  mz <- calculate_mz_from_mass(neutral_mass, "[M-H]-")
  mass_back <- calculate_mass_of_m(mz = mz, adduct_string = "[M-H]-")

  expect_equal(mass_back, neutral_mass, tolerance = 1e-6)
})

test_that("mass -> mz -> mass round-trip works for dimer", {
  neutral_mass <- 100.0

  mz <- calculate_mz_from_mass(neutral_mass, "[2M+H]+")
  mass_back <- calculate_mass_of_m(mz = mz, adduct_string = "[2M+H]+")

  expect_equal(mass_back, neutral_mass, tolerance = 1e-4)
})

test_that("mass -> mz -> mass round-trip works for doubly charged", {
  neutral_mass <- 500.0

  mz <- calculate_mz_from_mass(neutral_mass, "[M+2H]2+")
  mass_back <- calculate_mass_of_m(mz, "[M+2H]2+")

  expect_equal(mass_back, neutral_mass, tolerance = 1e-4)
})

test_that("mass -> mz -> mass round-trip works for isotopologue", {
  neutral_mass <- 200.0

  mz <- calculate_mz_from_mass(neutral_mass, "[M1+H]+")
  mass_back <- calculate_mass_of_m(mz, "[M1+H]+")

  expect_equal(mass_back, neutral_mass, tolerance = 1e-4)
})

test_that("mass -> mz -> mass round-trip works with water loss", {
  neutral_mass <- 180.0634

  mz <- calculate_mz_from_mass(neutral_mass, "[M+H-H2O]+")
  mass_back <- calculate_mass_of_m(mz, "[M+H-H2O]+")

  expect_equal(mass_back, neutral_mass, tolerance = 1e-4)
})

## Round-trip with multiple adducts ----

test_that("round-trip works for all common positive adducts", {
  neutral_mass <- 150.5
  adducts <- c("[M+H]+", "[M+Na]+", "[M+K]+", "[M+NH4]+", "[2M+H]+")

  for (adduct in adducts) {
    mz <- calculate_mz_from_mass(neutral_mass, adduct)
    mass_back <- calculate_mass_of_m(mz, adduct)

    expect_equal(mass_back, neutral_mass, tolerance = 1e-6, label = adduct)
  }
})

test_that("round-trip works for all common negative adducts", {
  neutral_mass <- 175.3
  adducts <- c("[M-H]-", "[M+Cl]-", "[M+HCOO]-")

  for (adduct in adducts) {
    mz <- calculate_mz_from_mass(neutral_mass, adduct)
    mass_back <- calculate_mass_of_m(mz, adduct)

    expect_equal(mass_back, neutral_mass, tolerance = 1e-6, label = adduct)
  }
})

## High precision round-trip ----

test_that("round-trip maintains high precision", {
  neutral_mass <- 194.080375570 # High precision caffeine

  mz <- calculate_mz_from_mass(neutral_mass, "[M+H]+")
  mass_back <- calculate_mass_of_m(mz, "[M+H]+")

  # Should maintain at least 9 decimal places
  expect_equal(mass_back, neutral_mass, tolerance = 1e-9)
})

## Multiple round-trips ----

test_that("multiple round-trips don't accumulate error", {
  neutral_mass <- 250.0

  mass <- neutral_mass
  for (i in 1:10) {
    mz <- calculate_mz_from_mass(mass, "[M+H]+")
    mass <- calculate_mass_of_m(mz, "[M+H]+")
  }

  expect_equal(mass, neutral_mass, tolerance = 1e-6)
})

## Round-trip with different mass ranges ----

test_that("round-trip works for small molecules", {
  masses <- c(50, 100, 200)

  for (mass in masses) {
    mz <- calculate_mz_from_mass(mass, "[M+H]+")
    mass_back <- calculate_mass_of_m(mz, "[M+H]+")

    expect_equal(
      mass_back,
      mass,
      tolerance = 1e-6,
      label = paste("Mass:", mass)
    )
  }
})

test_that("round-trip works for large molecules", {
  masses <- c(500, 1000, 2000)

  for (mass in masses) {
    mz <- calculate_mz_from_mass(mass, "[M+H]+")
    mass_back <- calculate_mass_of_m(mz, "[M+H]+")

    expect_equal(
      mass_back,
      mass,
      tolerance = 1e-4,
      label = paste("Mass:", mass)
    )
  }
})

## Round-trip matrix (mass × adduct) ----

test_that("round-trip works for matrix of masses and adducts", {
  masses <- c(100, 200, 500)
  adducts <- c("[M+H]+", "[M+Na]+", "[2M+H]+")

  for (mass in masses) {
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

## Logging ----

test_that("calculate_mz_from_mass completes without error", {
  # Should complete successfully for valid input
  expect_silent(calculate_mz_from_mass(100, "[M+H]+"))
})

## Performance ----

test_that("calculate_mz_from_mass is fast for batch processing", {
  skip_on_cran()

  masses <- runif(1000, 100, 1000)

  start_time <- Sys.time()
  mz_values <- sapply(masses, function(mass) {
    calculate_mz_from_mass(mass, "[M+H]+")
  })
  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")

  expect_true(elapsed < 5.0)
  expect_equal(length(mz_values), 1000)
})

test_that("round-trip calculation is fast", {
  skip_on_cran()

  masses <- runif(100, 100, 500)

  start_time <- Sys.time()
  for (mass in masses) {
    mz <- calculate_mz_from_mass(mass, "[M+H]+")
    mass_back <- calculate_mass_of_m(mz, "[M+H]+")
  }
  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")

  expect_true(elapsed < 5.0)
})
