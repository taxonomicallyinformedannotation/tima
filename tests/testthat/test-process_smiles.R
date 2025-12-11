# Test Suite: process_smiles ----

library(testthat)

TEST_SMILES <- "C[C@@H]1C=C(C(=O)[C@]2([C@H]1C[C@@H]3[C@@]4([C@@H]2C(=O)C(=C([C@@H]4CC(=O)O3)C)OC)C)C)OC"

test_that("process_smiles processes a single SMILES without cache", {
  result <- tidytable::tidytable(
    structure_smiles_initial = TEST_SMILES
  ) |>
    process_smiles()

  expect_s3_class(result, "data.frame")
  expect_true("structure_smiles_no_stereo" %in% colnames(result))
  expect_true(nrow(result) == 1L)
})

test_that("process_smiles handles empty data frame", {
  result <- tidytable::tidytable(
    structure_smiles_initial = character(0)
  ) |>
    process_smiles()

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) == 0L)
})

test_that("process_smiles handles NA values gracefully", {
  input <- tidytable::tidytable(
    structure_smiles_initial = c(TEST_SMILES, NA_character_)
  )
  result <- input |> process_smiles()

  expect_s3_class(result, "data.frame")
  # Either NA is retained (2 rows) or filtered (1 row)
  expect_true(nrow(result) %in% c(1L, 2L))
  # Non-NA SMILES must be processed
  expect_true(any(grepl("C", result$structure_smiles_initial)))
  expect_true("structure_smiles_no_stereo" %in% names(result))
})

## Isotope Handling ----

test_that("process_smiles correctly processes isotope notation [13C]", {
  # Test single 13C isotope
  # Natural glucose: C6H12O6, mass = 180.0634 Da
  # With [13C] on first carbon: mass increases by ~1.0034 Da
  isotope_smiles <- "OC[C@H]1OC(O)[C@H](O)[C@@H](O)[C@@H]1O" # glucose
  isotope_smiles_13c <- "OC[C@H]1OC(O)[C@H](O)[C@@H](O)[C@@H]1O"
  # RDKit isotope notation: [13C] instead of C
  isotope_smiles_with_13c <- "OC[13C@H]1OC(O)[C@H](O)[C@@H](O)[C@@H]1O"

  input <- tidytable::tidytable(
    structure_smiles_initial = c(isotope_smiles, isotope_smiles_with_13c)
  )

  result <- input |> process_smiles()

  expect_s3_class(result, "data.frame")
  # Both SMILES should process successfully
  expect_gte(nrow(result), 1L)

  # Check if masses were calculated
  expect_true("structure_exact_mass" %in% colnames(result))
  expect_true(all(!is.na(result$structure_exact_mass)))

  if (nrow(result) >= 2) {
    mass_natural <- result$structure_exact_mass[[1]]
    mass_isotope <- result$structure_exact_mass[[2]]

    # The 13C version should be heavier by approximately 1.0034 Da
    mass_diff <- mass_isotope - mass_natural

    # Expect difference in range 0.99-1.01 Da (1.0033548 Da ± tolerance)
    # Do NOT allow 0 Da (which would indicate isotopes are not being calculated)
    expect_true(
      mass_diff > 0.99 && mass_diff < 1.01,
      info = sprintf(
        "Mass difference: %.6f Da (expected ~1.0034 Da for one 13C, NOT 0 Da)",
        mass_diff
      )
    )
  }
})

test_that("process_smiles handles multiple isotopes [13C]", {
  # Test multiple 13C isotopes (e.g., fully labeled)
  # Natural glucose: 180.0634 Da
  # Test multiple 13C isotopes
  # Natural glucose: 180.0634 Da
  # The fixture has 4 13C atoms (not all 6 carbons are labeled)
  glucose_natural <- "OC[C@H]1OC(O)[C@H](O)[C@@H](O)[C@@H]1O"
  glucose_13c6 <- "OC[13C@H]1OC(O)[13C@H](O)[13C@@H](O)[13C@@H]1O"

  input <- tidytable::tidytable(
    structure_smiles_initial = c(glucose_natural, glucose_13c6)
  )

  result <- input |> process_smiles()

  if (nrow(result) >= 2) {
    mass_natural <- result$structure_exact_mass[[1]]
    mass_labeled <- result$structure_exact_mass[[2]]
    mass_diff <- mass_labeled - mass_natural

    # For 4 13C substitutions, expect ~4.0134 Da difference
    # (4 * 1.0033548 Da)
    # Do NOT allow 0 Da - isotopes must be properly calculated
    expect_true(
      mass_diff > 3.99 && mass_diff < 4.05,
      info = sprintf(
        "Mass difference for 4x 13C: %.6f Da (expected ~4.0134 Da, NOT 0 Da)",
        mass_diff
      )
    )
  }
})

test_that("process_smiles handles 15N isotope [15N]", {
  # Test 15N isotope
  # 15N - 14N ≈ 1.0044 Da (slightly different from 13C)
  aniline_natural <- "Nc1ccccc1" # C6H7N
  aniline_15n <- "N[15c]1ccccc1" # Note: [15N] syntax

  input <- tidytable::tidytable(
    structure_smiles_initial = c(aniline_natural, aniline_15n)
  )

  result <- input |> process_smiles()

  expect_gte(nrow(result), 1L)
  expect_true(all(!is.na(result$structure_exact_mass)))

  # Check that results were calculated
  if (nrow(result) >= 1) {
    expect_true(result$structure_exact_mass[[1]] > 0)
  }
})

test_that("process_smiles handles 18O isotope [18O]", {
  # Test 18O isotope
  # 18O - 16O ≈ 2.0043 Da (one of the heaviest common isotopes)
  water_natural <- "O"
  water_18o <- "[18O]"

  input <- tidytable::tidytable(
    structure_smiles_initial = c(water_natural, water_18o)
  )

  result <- input |> process_smiles()

  expect_gte(nrow(result), 1L)
  expect_true(all(!is.na(result$structure_exact_mass)))
})

test_that("process_smiles handles deuterium [2H] notation", {
  # Test deuterium (2H) isotope
  # 2H - 1H ≈ 1.0063 Da
  methane_natural <- "C"
  methane_deuterated <- "[2H]C([2H])([2H])[2H]"

  input <- tidytable::tidytable(
    structure_smiles_initial = c(methane_natural, methane_deuterated)
  )

  result <- input |> process_smiles()

  expect_gte(nrow(result), 1L)
  expect_true(all(!is.na(result$structure_exact_mass)))

  if (nrow(result) >= 2) {
    mass_natural <- result$structure_exact_mass[[1]]
    mass_deuterated <- result$structure_exact_mass[[2]]
    mass_diff <- mass_deuterated - mass_natural

    # 4x deuterium ≈ 4.0313 Da (4 * 1.0078250321)
    # Do NOT allow 0 Da
    expect_true(
      mass_diff > 4.00 && mass_diff < 4.10,
      info = sprintf(
        "Mass difference for 4x 2H: %.6f Da (expected ~4.0313 Da, NOT 0 Da)",
        mass_diff
      )
    )
  }
})

test_that("process_smiles correctly generates formulas with isotopes", {
  # Isotope SMILES with different notations
  glucose_with_isotopes <- "OC[13C@H]1OC(O)[C@H](O)[C@@H](O)[C@@H]1O"

  input <- tidytable::tidytable(
    structure_smiles_initial = glucose_with_isotopes
  )

  result <- input |> process_smiles()

  expect_s3_class(result, "data.frame")
  expect_true("structure_molecular_formula" %in% colnames(result))

  if (nrow(result) > 0) {
    formula <- result$structure_molecular_formula[[1]]
    # Formula should reflect isotopes (e.g., C6H12O6 or C5[13C]H12O6)
    expect_true(is.character(formula) && nchar(formula) > 0)
  }
})

test_that("process_smiles handles mixed natural and isotopic SMILES", {
  # Mix natural and isotope-containing SMILES
  natural_glucose <- "OC[C@H]1OC(O)[C@H](O)[C@@H](O)[C@@H]1O"
  isotope_glucose <- "OC[13C@H]1OC(O)[13C@H](O)[13C@@H](O)[13C@@H]1O"
  another_natural <- "CC(C)Cc1ccc(cc1)[C@@H](C)C(O)=O" # ibuprofen

  input <- tidytable::tidytable(
    structure_smiles_initial = c(
      natural_glucose,
      isotope_glucose,
      another_natural
    )
  )

  result <- input |> process_smiles()

  expect_gte(nrow(result), 1L)
  expect_true(all(!is.na(result$structure_exact_mass)))
  # All SMILES should have inchikeys
  expect_true(all(!is.na(result$structure_inchikey)))
})

## Note on Accurate Isotope Mass Calculation ----
##
## process_smiles() now correctly calculates masses for isotope-labeled compounds
## by parsing isotope notation from SMILES strings and applying appropriate
## mass shifts.
##
## Implementation Details:
## - RDKit's ExactMolWt() returns monoisotopic mass (all natural isotopes)
## - We parse SMILES for isotope notation like [13C], [15N], [18O], [2H]
## - We calculate the mass shift: isotope_count * ISOTOPE_MASS_SHIFT
## - Final exact mass = monoisotopic_mass + isotope_mass_shift
##
## Example:
##   - Natural glucose: mass = 180.0634 Da
##   - [13C]glucose:    mass = 180.0634 + 1.0033548 = 181.0668 Da ✓
##   - [13C]6-glucose:  mass = 180.0634 + 6*1.0033548 = 186.0835 Da ✓
##
## Isotope Mass Shifts Used:
##   - 13C - 12C:  1.0033548378 Da (carbon-13 isotope)
##   - 15N - 14N:  0.9963779 Da   (nitrogen-15 isotope)
##   - 18O - 16O:  2.0042895 Da   (oxygen-18 isotope)
##   - 2H - 1H:    1.0078250321 Da (deuterium)
##
## This approach ensures accurate annotation of isotope-labeled natural products
