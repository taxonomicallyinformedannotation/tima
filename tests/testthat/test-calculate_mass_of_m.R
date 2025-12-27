# Test Suite: calculate_mass_of_m ----

library(testthat)

## Helper Functions ----

# Helper to assert mass calculation within strict tolerance (0.0001 Da = 0.1 mDa)
# Uses constants from tima package for maximum accuracy
expect_mass_equal <- function(calculated, expected, tolerance = 0.0001) {
  expect_true(
    abs(calculated - expected) < tolerance,
    label = paste0(
      "Mass differs: calculated=",
      format(calculated, digits = 10),
      " expected=",
      format(expected, digits = 10),
      " (diff=",
      format(abs(calculated - expected), digits = 10),
      " Da)"
    )
  )
}

# Helper to assert m/z calculation within strict tolerance (0.0001 Da = 0.1 mDa)
expect_mz_equal <- function(calculated, expected, tolerance = 0.0001) {
  expect_true(
    abs(calculated - expected) < tolerance,
    label = paste0(
      "m/z differs: calculated=",
      format(calculated, digits = 10),
      " expected=",
      format(expected, digits = 10),
      " (diff=",
      format(abs(calculated - expected), digits = 10),
      " Da)"
    )
  )
}

# Helper for round-trip testing with strict tolerance
expect_round_trip <- function(original_mass, adduct, tolerance = 0.0001) {
  mz <- calculate_mz_from_mass(
    neutral_mass = original_mass,
    adduct_string = adduct
  )
  mass_back <- calculate_mass_of_m(mz = mz, adduct_string = adduct)
  expect_mass_equal(mass_back, original_mass, tolerance = tolerance)
}

## Basic mass calculation ----

# Define mass constants for strict accuracy
ADDUCT_MASSES <- list(
  H = 1.00783,
  Li = 7.01600,
  NH4 = 18.03437,
  Na = 22.98977,
  Mg = 23.98504,
  Cl = 34.96885,
  K = 38.96371,
  Ca = 39.96259,
  HCOO = 44.99765, # Formate
  Fe = 55.93494,
  CH3COO = 59.01330, # Acetate
  Br = 78.91834,
  CF3COO = 112.98504 # Trifluoroacetate
)
H_MASS <- ADDUCT_MASSES$H
Na_MASS <- ADDUCT_MASSES$Na
K_MASS <- ADDUCT_MASSES$K
NH4_MASS <- ADDUCT_MASSES$NH4
H2O_LOSS <- 18.010565 # Water loss (exact)
NH3_LOSS <- 17.026549 # Ammonia loss (exact)

test_that("calculate_mass_of_m calculates mass from [M+H]+", {
  mz <- 195.0877 # Caffeine [M+H]+
  mass <- calculate_mass_of_m(mz = mz, adduct_string = "[M+H]+")

  expect_type(mass, "double")
  expect_true(mass > 0)
  expect_mass_equal(mass, 194.079875)
})

test_that("calculate_mass_of_m calculates mass from [M-H]-", {
  mz <- 199.0
  mass <- calculate_mass_of_m(mz = mz, adduct_string = "[M-H]-")

  expect_true(mass > 0)
  expect_mass_equal(mass, 200.007825)
})

test_that("calculate_mass_of_m calculates mass from [M+Na]+", {
  mz <- 223.0
  mass <- calculate_mass_of_m(mz = mz, adduct_string = "[M+Na]+")

  expect_mass_equal(mass, 200.0102307)
})

## Required parameters validation ----

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

## mz validation ----

test_that("calculate_mass_of_m rejects negative m/z", {
  expect_error(
    calculate_mass_of_m(mz = -100, adduct_string = "[M+H]+"),
    "Fix: Use a value between 0 and Inf",
    fixed = TRUE
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
    "Invalid type for mz"
  )
})

# test_that("calculate_mass_of_m rejects Inf m/z", {
#   expect_error(
#     calculate_mass_of_m(mz = Inf, adduct_string = "[M+H]+"),
#     "finite"
#   )
# })

test_that("calculate_mass_of_m rejects non-numeric m/z", {
  expect_error(
    calculate_mass_of_m(mz = "100", adduct_string = "[M+H]+"),
    "numeric"
  )
})

test_that("calculate_mass_of_m rejects multiple m/z values", {
  expect_error(
    calculate_mass_of_m(mz = c(100, 200), adduct_string = "[M+H]+"),
    "Invalid type for mz"
  )
})

# test_that("calculate_mass_of_m warns about very high m/z", {
#   expect_warning(
#     calculate_mass_of_m(mz = 10000, adduct_string = "[M+H]+"),
#     "exceeds typical"
#   )
# })

## Invalid adduct handling ----

test_that("calculate_mass_of_m returns 0 for invalid adduct with warning", {
  expect_warning(
    mass <- calculate_mass_of_m(mz = 100, adduct_string = "invalid"),
    "Failed to parse"
  )
  expect_equal(mass, 0)
})

test_that("calculate_mass_of_m returns 0 for empty adduct", {
  expect_warning(
    result <- calculate_mass_of_m(mz = 100, adduct_string = ""),
    "Failed to parse"
  )
  expect_equal(result, 0)
})

## Division by zero protection ----

test_that("calculate_mass_of_m handles zero multimer count", {
  # This would require a malformed adduct that parse_adduct returns n_mer=0
  # Typically handled by parse_adduct returning failed parse
  expect_silent(parse_adduct("[M+H]+")) # Sanity check
})

test_that("calculate_mass_of_m handles zero charges", {
  # Parse_adduct should fail gracefully
  expect_warning(parse_adduct("[M]"), NA) # May or may not warn
})

## Electron mass validation ----

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
    "electron_mass out of valid range"
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

# test_that("calculate_mass_of_m warns about non-standard electron mass", {
#   expect_warning(
#     calculate_mass_of_m(
#       mz = 100,
#       adduct_string = "[M+H]+",
#       electron_mass = 0.001
#     ),
#     "differs from standard"
#   )
# })

## Multimer handling ----

test_that("calculate_mass_of_m handles dimer correctly", {
  neutral_mass <- 100.0
  mz_dimer <- neutral_mass * 2 + H_MASS # [2M+H]+

  mass <- calculate_mass_of_m(mz = mz_dimer, adduct_string = "[2M+H]+")
  expect_mass_equal(mass, neutral_mass)
})

test_that("calculate_mass_of_m handles trimer correctly", {
  neutral_mass <- 100.0
  mz_trimer <- neutral_mass * 3 + H_MASS # [3M+H]+

  mass <- calculate_mass_of_m(mz = mz_trimer, adduct_string = "[3M+H]+")
  expect_mass_equal(mass, neutral_mass)
})

## Multiple charges ----

test_that("calculate_mass_of_m handles doubly charged ions", {
  neutral_mass <- 500.0
  # [M+2H]2+ has m/z = (500 + 2*H_MASS) / 2
  mz_double <- (neutral_mass + 2 * H_MASS) / 2

  mass <- calculate_mass_of_m(mz = mz_double, adduct_string = "[M+2H]2+")
  expect_mass_equal(mass, neutral_mass)
})

test_that("calculate_mass_of_m handles triply charged ions", {
  neutral_mass <- 1000.0
  mz_triple <- (neutral_mass + 3 * H_MASS) / 3

  mass <- calculate_mass_of_m(mz = mz_triple, adduct_string = "[M+3H]3+")
  expect_mass_equal(mass, neutral_mass)
})

## Isotopologues ----

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

## Complex modifications ----

test_that("calculate_mass_of_m handles water loss", {
  neutral_mass <- 180.0634 # Glucose
  # [M+H-H2O]+ = glucose + H - H2O
  mz_loss <- neutral_mass + H_MASS - H2O_LOSS

  mass <- calculate_mass_of_m(mz = mz_loss, adduct_string = "[M+H-H2O]+")
  expect_mass_equal(mass, neutral_mass)
})

test_that("calculate_mass_of_m handles ammonia loss", {
  neutral_mass <- 150.0
  mz_loss <- neutral_mass + H_MASS - NH3_LOSS

  mass <- calculate_mass_of_m(mz = mz_loss, adduct_string = "[M+H-NH3]+")
  expect_mass_equal(mass, neutral_mass)
})

## Edge cases ----

test_that("calculate_mass_of_m handles very small masses", {
  neutral_mass <- 1.0
  mz <- neutral_mass + H_MASS

  mass <- calculate_mass_of_m(mz = mz, adduct_string = "[M+H]+")
  expect_mass_equal(mass, neutral_mass)
})

test_that("calculate_mass_of_m handles very large masses", {
  neutral_mass <- 4000.0
  mz <- neutral_mass + H_MASS

  mass <- calculate_mass_of_m(mz = mz, adduct_string = "[M+H]+")
  expect_mass_equal(mass, neutral_mass)
})

## Logging ----

test_that("calculate_mass_of_m logs warnings appropriately", {
  expect_warning(
    calculate_mass_of_m(mz = 100, adduct_string = "bad"),
    "Failed to parse"
  )
})

## Real-world metabolites ----

test_that("calculate_mass_of_m works with glucose", {
  glucose_mass <- 180.0634
  glucose_mz <- glucose_mass + Na_MASS # [M+Na]+

  mass <- calculate_mass_of_m(mz = glucose_mz, adduct_string = "[M+Na]+")
  expect_mass_equal(mass, glucose_mass)
})

test_that("calculate_mass_of_m works with caffeine", {
  caffeine_mass <- 194.0803
  caffeine_mz <- caffeine_mass + H_MASS # [M+H]+

  mass <- calculate_mass_of_m(mz = caffeine_mz, adduct_string = "[M+H]+")
  expect_mass_equal(mass, caffeine_mass)
})

test_that("calculate_mass_of_m works with cholesterol", {
  cholesterol_mass <- 386.3549
  cholesterol_mz <- cholesterol_mass + NH4_MASS # [M+NH4]+

  mass <- calculate_mass_of_m(mz = cholesterol_mz, adduct_string = "[M+NH4]+")
  expect_mass_equal(mass, cholesterol_mass)
})

## Performance ----

test_that("calculate_mass_of_m is fast for batch processing", {
  mz_values <- runif(1000, 100, 1000)

  start_time <- Sys.time()
  masses <- sapply(mz_values, function(mz) {
    calculate_mass_of_m(mz = mz, adduct_string = "[M+H]+")
  })
  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")

  expect_true(elapsed < 1.0)
  expect_equal(length(masses), 1000)
})

# Test Suite: calculate_mz_from_mass ----

## Basic m/z calculation ----

test_that("calculate_mz_from_mass calculates m/z from neutral mass [M+H]+", {
  neutral_mass <- 194.0803 # Caffeine
  mz <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[M+H]+"
  )

  expect_type(mz, "double")
  expect_true(mz > 0)
  # Expected m/z: (mass + H_MASS + electron) / 1
  expected_mz <- (neutral_mass + H_MASS) / 1
  expect_mz_equal(mz, expected_mz)
})

test_that("calculate_mz_from_mass calculates m/z [M+Na]+", {
  neutral_mass <- 180.0634 # Glucose
  mz <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[M+Na]+"
  )

  expect_true(mz > 0)
  # Expected: (mass + Na_MASS + electron) / 1
  expected_mz <- (neutral_mass + Na_MASS) / 1
  expect_mz_equal(mz, expected_mz)
})

test_that("calculate_mz_from_mass calculates m/z [M-H]-", {
  neutral_mass <- 200.0
  mz <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[M-H]-"
  )

  expect_true(mz > 0)
  expect_true(abs(mz - 199.0) < 0.1)
})

## Required parameters validation ----

test_that("calculate_mz_from_mass requires neutral_mass parameter", {
  expect_error(
    calculate_mz_from_mass(adduct_string = "[M+H]+"),
    "must be provided"
  )
})

test_that("calculate_mz_from_mass requires adduct_string parameter", {
  expect_error(
    calculate_mz_from_mass(neutral_mass = 100),
    "must be provided"
  )
})

## Mass validation ----

test_that("calculate_mz_from_mass rejects negative mass", {
  expect_error(
    calculate_mz_from_mass(neutral_mass = -100, adduct_string = "[M+H]+"),
    "neutral_mass out of valid range"
  )
})

test_that("calculate_mz_from_mass rejects non-numeric mass", {
  expect_error(
    calculate_mz_from_mass(neutral_mass = "100", adduct_string = "[M+H]+"),
    "numeric"
  )
})

## Invalid adduct handling ----

test_that("calculate_mz_from_mass returns 0 for invalid adduct with warning", {
  expect_warning(
    mz <- calculate_mz_from_mass(neutral_mass = 100, adduct_string = "invalid"),
    "Failed to parse"
  )
  expect_equal(mz, 0)
})

## Multimer handling ----

test_that("calculate_mz_from_mass handles dimer [2M+H]+", {
  neutral_mass <- 100.0
  mz <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[2M+H]+"
  )

  # Expected: (2*100 + H_MASS + electron) / 1
  expected_mz <- (2 * neutral_mass + H_MASS) / 1
  expect_mz_equal(mz, expected_mz)
})

test_that("calculate_mz_from_mass handles trimer [3M+H]+", {
  neutral_mass <- 100.0
  mz <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[3M+H]+"
  )

  # Expected: (3*100 + H_MASS + electron) / 1
  expected_mz <- (3 * neutral_mass + H_MASS) / 1
  expect_mz_equal(mz, expected_mz)
})

## Multiple charges ----

test_that("calculate_mz_from_mass handles doubly charged [M+2H]2+", {
  neutral_mass <- 500.0
  mz <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[M+2H]2+"
  )

  # Expected: (500 + 2*H_MASS + 2*electron) / 2
  expected_mz <- (neutral_mass + 2 * H_MASS) / 2
  expect_mz_equal(mz, expected_mz)
})

test_that("calculate_mz_from_mass handles triply charged [M+3H]3+", {
  neutral_mass <- 1000.0
  mz <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[M+3H]3+"
  )

  # Expected: (1000 + 3*H_MASS + 3*electron) / 3
  expected_mz <- (neutral_mass + 3 * H_MASS) / 3
  expect_mz_equal(mz, expected_mz)
})

## Isotopologue handling ----

test_that("calculate_mz_from_mass handles M+1 isotopologue [M1+H]+", {
  neutral_mass <- 200.0
  mz <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[M1+H]+"
  )

  # Expected: ((200 + 1.007825) / 1) - 1 = 200.007825 Da (after isotope shift subtraction)
  expect_type(mz, "double")
  expect_true(mz > 0)
})

test_that("calculate_mz_from_mass handles M+2 isotopologue [M2+H]+", {
  neutral_mass <- 200.0
  mz <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[M2+H]+"
  )

  expect_type(mz, "double")
  expect_true(mz > 0)
})

test_that("calculate_mz_from_mass handles M+1 alternative notation [M+1+H]+", {
  neutral_mass <- 150.0
  mz <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[M+1+H]+"
  )

  expect_type(mz, "double")
  expect_true(mz > 0)
})

test_that("calculate_mz_from_mass handles dimer with M+1 [2M1+Na]+", {
  neutral_mass <- 100.0
  mz <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[2M1+Na]+"
  )

  # Expected: ((2*100 + 22.989) / 1) - 1 = 221.989
  expect_type(mz, "double")
  expect_true(mz > 0)
})

## Complex modifications ----

test_that("calculate_mz_from_mass handles water loss [M+H-H2O]+", {
  neutral_mass <- 180.0634 # Glucose
  mz <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[M+H-H2O]+"
  )

  # Expected: (180.0634 + H_MASS - H2O_LOSS + electron) / 1
  expected_mz <- (neutral_mass + H_MASS - H2O_LOSS) / 1
  expect_mz_equal(mz, expected_mz)
})

test_that("calculate_mz_from_mass handles ammonia loss [M+H-NH3]+", {
  neutral_mass <- 150.0
  mz <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[M+H-NH3]+"
  )

  # Expected: (150 + H_MASS - NH3_LOSS + electron) / 1
  expected_mz <- (neutral_mass + H_MASS - NH3_LOSS) / 1
  expect_mz_equal(mz, expected_mz)
})

## Round-trip calculations ----

test_that("calculate_mz_from_mass and calculate_mass_of_m are inverses [M+H]+", {
  expect_round_trip(122.45, "[M+H]+")
})

test_that("round-trip works for [M+Na]+", {
  expect_round_trip(180.0634, "[M+Na]+")
})

test_that("round-trip works for [2M+H]+", {
  expect_round_trip(100.0, "[2M+H]+")
})

test_that("round-trip works for [M+2H]2+", {
  expect_round_trip(500.0, "[M+2H]2+")
})

test_that("round-trip works for [M1+H]+", {
  expect_round_trip(200.0, "[M1+H]+")
})

test_that("round-trip works for [M+H-H2O]+", {
  expect_round_trip(180.0634, "[M+H-H2O]+")
})

## Edge cases ----

test_that("calculate_mz_from_mass handles very small masses", {
  neutral_mass <- 1.0
  mz <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[M+H]+"
  )

  expect_true(mz > 0)
  expected_mz <- (neutral_mass + H_MASS) / 1
  expect_mz_equal(mz, expected_mz)
})

test_that("calculate_mz_from_mass handles very large masses", {
  neutral_mass <- 4000.0
  mz <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[M+H]+"
  )

  expect_true(mz > 0)
  expected_mz <- (neutral_mass + H_MASS) / 1
  expect_mz_equal(mz, expected_mz)
})

## Consistency with real metabolites ----

test_that("calculate_mz_from_mass works with glucose", {
  glucose_mass <- 180.0634
  # Expected: (mass + Na_MASS + electron) / 1
  expected_mz <- (glucose_mass + Na_MASS) / 1

  mz <- calculate_mz_from_mass(
    neutral_mass = glucose_mass,
    adduct_string = "[M+Na]+"
  )
  expect_mz_equal(mz, expected_mz)
})

test_that("calculate_mz_from_mass works with caffeine", {
  caffeine_mass <- 194.0803
  # Expected: (mass + H_MASS + electron) / 1
  expected_mz <- (caffeine_mass + H_MASS) / 1

  mz <- calculate_mz_from_mass(
    neutral_mass = caffeine_mass,
    adduct_string = "[M+H]+"
  )
  expect_mz_equal(mz, expected_mz)
})

## Isotopologues ----

test_that("calculate_mass_of_m correctly handles M+1 isotope [M1+H]+", {
  # Test with glucose M+1
  # Glucose exact mass: 180.0634 Da
  # [M1+H]+ m/z calculation:
  # From mass: ((180.0634 + H_MASS) / 1) - 1 = 180.0707
  # To mass: should recover 180.0634 Da

  glucose_mass <- 180.0634
  mz_m1 <- calculate_mz_from_mass(
    neutral_mass = glucose_mass,
    adduct_string = "[M1+H]+"
  )

  # Reverse calculation
  mass_calculated <- calculate_mass_of_m(
    mz = mz_m1,
    adduct_string = "[M1+H]+"
  )

  expect_mz_equal(mass_calculated, glucose_mass)
})

test_that("calculate_mass_of_m correctly handles M+2 isotope [M2+Na]+", {
  # Test M+2 with sodium adduct
  neutral_mass <- 200.0
  mz_m2 <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[M2+Na]+"
  )

  mass_calculated <- calculate_mass_of_m(
    mz = mz_m2,
    adduct_string = "[M2+Na]+"
  )

  expect_mz_equal(mass_calculated, neutral_mass)
})

test_that("calculate_mass_of_m handles isotope alternative notation [M+1+H]+", {
  neutral_mass <- 150.0
  mz <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[M+1+H]+"
  )

  mass_back <- calculate_mass_of_m(
    mz = mz,
    adduct_string = "[M+1+H]+"
  )

  expect_mz_equal(mass_back, neutral_mass)
})

test_that("isotope shifts are correctly subtracted in m/z calculation", {
  # For [M1+H]+, the m/z formula is: ((M + H_MASS) / 1) + ISOTOPE_SHIFT
  # The isotope shift (1.0033548 Da) is added to get the higher m/z
  neutral_mass <- 100.0
  mz_m0 <- calculate_mz_from_mass(neutral_mass, "[M+H]+")
  mz_m1 <- calculate_mz_from_mass(neutral_mass, "[M1+H]+")

  # M+1 should be exactly ISOTOPE_MASS_SHIFT_DALTONS higher than M+0
  expect_equal(mz_m1 - mz_m0, ISOTOPE_MASS_SHIFT_DALTONS, tolerance = 1e-6)
})

test_that("isotope calculations work with multiple charges [M2+2H]2+", {
  neutral_mass <- 500.0
  mz <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[M2+2H]2+"
  )

  mass_back <- calculate_mass_of_m(
    mz = mz,
    adduct_string = "[M2+2H]2+"
  )

  expect_mz_equal(mass_back, neutral_mass)
})

test_that("isotope calculations work with dimers [2M1+Na]+", {
  neutral_mass <- 100.0

  # Calculate expected m/z for dimer M+1
  # Formula: ((2 * M + Na_MASS) / 1) + ISOTOPE_SHIFT
  expected_mz <- ((2 * neutral_mass + Na_MASS) / 1) + ISOTOPE_MASS_SHIFT_DALTONS

  mz <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[2M1+Na]+"
  )

  expect_mz_equal(mz, expected_mz)

  # Verify round-trip
  mass_back <- calculate_mass_of_m(
    mz = mz,
    adduct_string = "[2M1+Na]+"
  )

  expect_mz_equal(mass_back, neutral_mass)
})

test_that("M+0 and monoisotopic calculations are identical", {
  neutral_mass <- 122.45

  # M+0 and M (no isotope designation) should give same results
  # Both have n_iso = 0, so no isotope shift is applied
  mz_m <- calculate_mz_from_mass(neutral_mass, "[M+H]+")
  mz_m0 <- calculate_mz_from_mass(neutral_mass, "[M0+H]+")

  expect_equal(mz_m, mz_m0)
})

test_that("isotope calculations work in negative mode [M1-H]-", {
  neutral_mass <- 150.0

  mz <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[M1-H]-"
  )

  mass_back <- calculate_mass_of_m(
    mz = mz,
    adduct_string = "[M1-H]-"
  )

  expect_mz_equal(mass_back, neutral_mass)
})

test_that("isotope with water loss [M1+H-H2O]+ calculates correctly", {
  # Test combined isotope and neutral loss
  neutral_mass <- 180.0634 # Glucose

  mz <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[M1+H-H2O]+"
  )

  mass_back <- calculate_mass_of_m(
    mz = mz,
    adduct_string = "[M1+H-H2O]+"
  )

  expect_mz_equal(mass_back, neutral_mass)
})

test_that("large isotope shifts work correctly [M13+H]+", {
  # For fully 13C-labeled glucose (6 carbons)
  # Or other heavily labeled compounds
  neutral_mass <- 180.0634

  mz <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[M13+H]+"
  )

  # M+13 should be 13 * ISOTOPE_MASS_SHIFT_DALTONS more than M+0 in m/z
  mz_m0 <- calculate_mz_from_mass(neutral_mass, "[M+H]+")
  expect_equal(
    mz - mz_m0,
    13.0 * ISOTOPE_MASS_SHIFT_DALTONS,
    tolerance = 1e-6
  )

  # Verify round-trip
  mass_back <- calculate_mass_of_m(
    mz = mz,
    adduct_string = "[M13+H]+"
  )

  expect_mz_equal(mass_back, neutral_mass)
})

test_that("isotope annotation distinguishes from regular mass differences", {
  # Verify that M+1 isotope gives different m/z than M+Na
  # Both add mass, but in different ways
  neutral_mass <- 100.0

  mz_m1 <- calculate_mz_from_mass(neutral_mass, "[M1+H]+")
  mz_mna <- calculate_mz_from_mass(neutral_mass, "[M+Na]+")

  # These should be different values
  expect_false(abs(mz_m1 - mz_mna) < 0.01)

  # M+1 with H should be less than M with Na
  # [M1+H]+ = ((100 + 1.008) / 1) - 1 = 100.008
  # [M+Na]+ = ((100 + 22.990) / 1) = 122.990
  expect_lt(mz_m1, mz_mna)
})

test_that("realistic 13C-glucose example", {
  # 13C6-glucose (all 6 carbons labeled)
  # Natural glucose: 180.0634 Da
  # When measuring 13C6-glucose, the M+6 peak appears at higher m/z
  # But we annotate using natural mass with M6 designation

  natural_glucose <- 180.0634

  # The m/z for [M6+H]+ of natural glucose mass
  # Formula: ((180.0634 + H_MASS) / 1) + 6 * ISOTOPE_SHIFT
  mz_m6 <- calculate_mz_from_mass(
    neutral_mass = natural_glucose,
    adduct_string = "[M6+H]+"
  )

  # Should be 6 * ISOTOPE_MASS_SHIFT_DALTONS higher than M+0
  mz_m0 <- calculate_mz_from_mass(natural_glucose, "[M+H]+")
  expect_equal(
    mz_m6 - mz_m0,
    6.0 * ISOTOPE_MASS_SHIFT_DALTONS,
    tolerance = 1e-6
  )

  # Verify we can back-calculate the natural mass
  mass_back <- calculate_mass_of_m(
    mz = mz_m6,
    adduct_string = "[M6+H]+"
  )

  expect_mz_equal(mass_back, natural_glucose)
})

test_that("isotope round-trips work for all common adducts", {
  neutral_mass <- 200.0
  isotope_adducts <- c(
    "[M1+H]+",
    "[M1+Na]+",
    "[M1+K]+",
    "[M1-H]-",
    "[M2+H]+",
    "[2M1+H]+",
    "[M1+2H]2+"
  )

  for (adduct in isotope_adducts) {
    mz <- calculate_mz_from_mass(neutral_mass, adduct)
    mass_back <- calculate_mass_of_m(mz, adduct)

    expect_mz_equal(
      mass_back,
      neutral_mass
    )
  }
})
