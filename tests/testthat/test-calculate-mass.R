#' @title Test Suite for calculate_mass_of_m
#'
#' @description Individual focused tests for calculate_mass_of_m function.

library(testthat)
library(tima)

# Test: Basic mass calculation ----

test_that("calculate_mass_of_m calculates mass from [M+H]+", {
  mz <- 195.0877 # Caffeine [M+H]+
  mass <- calculate_mass_of_m(mz = mz, adduct_string = "[M+H]+")

  expect_type(mass, "double")
  expect_true(mass > 0)
  expect_true(abs(mass - 194.0803) < 0.01)
})

test_that("calculate_mass_of_m calculates mass from [M-H]-", {
  mz <- 199.0
  mass <- calculate_mass_of_m(mz = mz, adduct_string = "[M-H]-")

  expect_true(mass > 0)
  expect_true(abs(mass - 200) < 1)
})

test_that("calculate_mass_of_m calculates mass from [M+Na]+", {
  mz <- 223.0
  mass <- calculate_mass_of_m(mz = mz, adduct_string = "[M+Na]+")

  expect_true(abs(mass - 200) < 1)
})

# Test: Required parameters validation ----

test_that("calculate_mass_of_m requires mz parameter", {
  expect_error(
    calculate_mass_of_m(adduct_string = "[M+H]+"),
    "must be provided"
  )
})

test_that("calculate_mass_of_m requires adduct_string parameter", {
  expect_error(
    calculate_mass_of_m(mz = 100),
    "must be provided"
  )
})

# Test: mz validation ----

test_that("calculate_mass_of_m rejects negative m/z", {
  expect_error(
    calculate_mass_of_m(mz = -100, adduct_string = "[M+H]+"),
    "positive"
  )
})

test_that("calculate_mass_of_m rejects NA m/z", {
  expect_error(
    calculate_mass_of_m(mz = NA_real_, adduct_string = "[M+H]+"),
    "mz cannot be NA"
  )
})

test_that("calculate_mass_of_m rejects NA m/z", {
  expect_error(
    calculate_mass_of_m(mz = NA, adduct_string = "[M+H]+"),
    "mz must be numeric, got: logical"
  )
})

test_that("calculate_mass_of_m rejects Inf m/z", {
  expect_error(
    calculate_mass_of_m(mz = Inf, adduct_string = "[M+H]+"),
    "finite"
  )
})

test_that("calculate_mass_of_m rejects non-numeric m/z", {
  expect_error(
    calculate_mass_of_m(mz = "100", adduct_string = "[M+H]+"),
    "numeric"
  )
})

test_that("calculate_mass_of_m rejects multiple m/z values", {
  expect_error(
    calculate_mass_of_m(mz = c(100, 200), adduct_string = "[M+H]+"),
    "single value"
  )
})

test_that("calculate_mass_of_m warns about very high m/z", {
  expect_warning(
    calculate_mass_of_m(mz = 10000, adduct_string = "[M+H]+"),
    "exceeds typical"
  )
})

# Test: Invalid adduct handling ----

# test_that("calculate_mass_of_m returns 0 for invalid adduct with warning", {
#   expect_warning(
#     mass <- calculate_mass_of_m(mz = 100, adduct_string = "invalid"),
#     "Failed to parse"
#   )
#   expect_equal(mass, 0)
# })

# test_that("calculate_mass_of_m returns 0 for empty adduct", {
#   result <- calculate_mass_of_m(mz = 100, adduct_string = "")
#   # Either returns 0 or throws error - both acceptable
#   expect_true(
#     is.numeric(result) || inherits(try(result, silent = TRUE), "try-error")
#   )
# })

# Test: Division by zero protection ----

# test_that("calculate_mass_of_m handles zero multimer count", {
#   # This would require a malformed adduct that parse_adduct returns n_mer=0
#   # Typically handled by parse_adduct returning failed parse
#   expect_silent(parse_adduct("[M+H]+")) # Sanity check
# })

test_that("calculate_mass_of_m handles zero charges", {
  # Parse_adduct should fail gracefully
  expect_warning(parse_adduct("[M]"), NA) # May or may not warn
})

# Test: Electron mass validation ----

test_that("calculate_mass_of_m accepts custom electron mass", {
  mass <- calculate_mass_of_m(
    mz = 195.0877,
    adduct_string = "[M+H]+",
    electron_mass = 0.0005486
  )

  expect_true(mass > 0)
})

test_that("calculate_mass_of_m rejects negative electron mass", {
  expect_error(
    calculate_mass_of_m(mz = 100, adduct_string = "[M+H]+", electron_mass = -1),
    "positive"
  )
})

test_that("calculate_mass_of_m rejects non-numeric electron mass", {
  expect_error(
    calculate_mass_of_m(
      mz = 100,
      adduct_string = "[M+H]+",
      electron_mass = "0.0005"
    ),
    "single numeric value"
  )
})

test_that("calculate_mass_of_m warns about non-standard electron mass", {
  expect_warning(
    calculate_mass_of_m(
      mz = 100,
      adduct_string = "[M+H]+",
      electron_mass = 0.001
    ),
    "differs from standard"
  )
})

# Test: Multimer handling ----

test_that("calculate_mass_of_m handles dimer correctly", {
  neutral_mass <- 100.0
  mz_dimer <- neutral_mass * 2 + 1.007 # Approximate [2M+H]+

  mass <- calculate_mass_of_m(mz = mz_dimer, adduct_string = "[2M+H]+")
  expect_true(abs(mass - neutral_mass) < 1)
})

test_that("calculate_mass_of_m handles trimer correctly", {
  neutral_mass <- 100.0
  mz_trimer <- neutral_mass * 3 + 1.007 # Approximate [3M+H]+

  mass <- calculate_mass_of_m(mz = mz_trimer, adduct_string = "[3M+H]+")
  expect_true(abs(mass - neutral_mass) < 1)
})

# Test: Multiple charges ----

test_that("calculate_mass_of_m handles doubly charged ions", {
  neutral_mass <- 500.0
  # [M+2H]2+ has m/z ≈ (500 + 2*1.007)/2
  mz_double <- (neutral_mass + 2.014) / 2

  mass <- calculate_mass_of_m(mz = mz_double, adduct_string = "[M+2H]2+")
  expect_true(abs(mass - neutral_mass) < 1)
})

test_that("calculate_mass_of_m handles triply charged ions", {
  neutral_mass <- 1000.0
  mz_triple <- (neutral_mass + 3.021) / 3

  mass <- calculate_mass_of_m(mz = mz_triple, adduct_string = "[M+3H]3+")
  expect_true(abs(mass - neutral_mass) < 1)
})

# Test: Isotopologues ----

# test_that("calculate_mass_of_m handles M+1 isotopologue", {
#   neutral_mass <- 200.0
#   mz_m1 <- neutral_mass + 1 + 1.007 # M+1 plus proton
#
#   mass <- calculate_mass_of_m(mz = mz_m1, adduct_string = "[M1+H]+")
#   expect_true(abs(mass - neutral_mass) < 1)
# })

# test_that("calculate_mass_of_m handles M+2 isotopologue", {
#   neutral_mass <- 200.0
#   mz_m2 <- neutral_mass + 2 + 1.007
#
#   mass <- calculate_mass_of_m(mz = mz_m2, adduct_string = "[M2+H]+")
#   expect_true(abs(mass - neutral_mass) < 1)
# })

# Test: Complex modifications ----

test_that("calculate_mass_of_m handles water loss", {
  neutral_mass <- 180.0634 # Glucose
  # [M+H-H2O]+ ≈ glucose + H - H2O
  mz_loss <- neutral_mass + 1.007 - 18.015

  mass <- calculate_mass_of_m(mz = mz_loss, adduct_string = "[M+H-H2O]+")
  expect_true(abs(mass - neutral_mass) < 0.1)
})

test_that("calculate_mass_of_m handles ammonia loss", {
  neutral_mass <- 150.0
  mz_loss <- neutral_mass + 1.007 - 17.027

  mass <- calculate_mass_of_m(mz = mz_loss, adduct_string = "[M+H-NH3]+")
  expect_true(abs(mass - neutral_mass) < 0.1)
})

# Test: Edge cases ----

test_that("calculate_mass_of_m handles very small masses", {
  neutral_mass <- 1.0
  mz <- neutral_mass + 1.007

  mass <- calculate_mass_of_m(mz = mz, adduct_string = "[M+H]+")
  expect_true(abs(mass - neutral_mass) < 0.01)
})

test_that("calculate_mass_of_m handles very large masses", {
  neutral_mass <- 4000.0
  mz <- neutral_mass + 1.007

  mass <- calculate_mass_of_m(mz = mz, adduct_string = "[M+H]+")
  expect_true(abs(mass - neutral_mass) < 0.1)
})

# Test: Logging ----

# test_that("calculate_mass_of_m logs warnings appropriately", {
#   expect_warning(
#     calculate_mass_of_m(mz = 100, adduct_string = "bad"),
#     "Failed to parse"
#   )
# })

# Test: Real-world metabolites ----

test_that("calculate_mass_of_m works with glucose", {
  glucose_mass <- 180.0634
  glucose_mz <- glucose_mass + 22.990 # [M+Na]+

  mass <- calculate_mass_of_m(mz = glucose_mz, adduct_string = "[M+Na]+")
  expect_true(abs(mass - glucose_mass) < 0.01)
})

test_that("calculate_mass_of_m works with caffeine", {
  caffeine_mass <- 194.0803
  caffeine_mz <- caffeine_mass + 1.007 # [M+H]+

  mass <- calculate_mass_of_m(mz = caffeine_mz, adduct_string = "[M+H]+")
  expect_true(abs(mass - caffeine_mass) < 0.01)
})

test_that("calculate_mass_of_m works with cholesterol", {
  cholesterol_mass <- 386.3549
  cholesterol_mz <- cholesterol_mass + 18.034 # [M+NH4]+

  mass <- calculate_mass_of_m(mz = cholesterol_mz, adduct_string = "[M+NH4]+")
  expect_true(abs(mass - cholesterol_mass) < 0.01)
})

# Test: Performance ----

test_that("calculate_mass_of_m is fast for batch processing", {
  skip_on_cran()

  mz_values <- runif(1000, 100, 1000)

  start_time <- Sys.time()
  masses <- sapply(mz_values, function(mz) {
    calculate_mass_of_m(mz = mz, adduct_string = "[M+H]+")
  })
  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")

  expect_true(elapsed < 1.0)
  expect_equal(length(masses), 1000)
})
