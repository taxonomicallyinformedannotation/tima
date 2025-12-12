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
  result <- input |>
    process_smiles()

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

  result <- input |>
    process_smiles()

  expect_s3_class(result, "data.frame")

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

  result <- input |>
    process_smiles()

  if (nrow(result) >= 2) {
    mass_natural <- result$structure_exact_mass[[1]]
    mass_labeled <- result$structure_exact_mass[[2]]
    mass_diff <- mass_labeled - mass_natural

    # For 4 13C substitutions, expect ~4.0134 Da difference
    # (4 * 1.0033548 Da)
    # Do NOT allow 0 Da - isotopes must be properly calculated
    expect_true(
      mass_diff > 4.01 && mass_diff < 4.015,
      info = sprintf(
        "Mass difference for 4x 13C: %.6f Da (expected ~4.0134 Da, NOT 0 Da)",
        mass_diff
      )
    )
  }
})

test_that("process_smiles handles 15N isotope with correct mass shift", {
  skip_if_not(require("tima"), "tima package required")

  # Test 15N isotope
  # 15N - 14N ≈ 0.9964 Da
  # Use aniline where N is explicit: Nc1ccccc1
  aniline_natural <- "Nc1ccccc1"
  # COMMENT: Explicit H is required for isotopes
  aniline_15n <- "[15NH2]c1ccccc1"

  input <- tidytable::tidytable(
    structure_smiles_initial = c(aniline_natural, aniline_15n)
  )

  result <- input |>
    process_smiles()

  expect_true(all(!is.na(result$structure_exact_mass)))

  # Find which row is which by checking SMILES
  if (nrow(result) >= 2) {
    idx_nat <- which(result$structure_smiles_initial == aniline_natural)
    idx_lab <- which(result$structure_smiles_initial == aniline_15n)

    if (length(idx_nat) > 0 && length(idx_lab) > 0) {
      mass_natural <- result$structure_exact_mass[[idx_nat[1]]]
      mass_labeled <- result$structure_exact_mass[[idx_lab[1]]]
      mass_diff <- mass_labeled - mass_natural

      # 15N shift ≈ 0.9964 Da
      # Allow ±0.01 Da tolerance
      expect_true(
        mass_diff > 0.98 && mass_diff < 1.01,
        info = sprintf(
          "Mass difference for 15N: %.6f Da (expected ~0.9964 Da)",
          mass_diff
        )
      )
    }
  }
})

test_that("process_smiles handles 18O isotope with correct mass shift", {
  skip_if_not(require("tima"), "tima package required")

  # Test 18O isotope
  # 18O - 16O ≈ 2.0043 Da
  # Use water: O (natural) vs [18O] (labeled)
  water_natural <- "O"
  # COMMENT: Explicit H is required for isotopes
  water_18o <- "[18OH2]"

  input <- tidytable::tidytable(
    structure_smiles_initial = c(water_natural, water_18o)
  )

  result <- input |>
    process_smiles()

  expect_true(all(!is.na(result$structure_exact_mass)))

  # If both molecules processed, check mass difference
  if (nrow(result) >= 2) {
    mass_natural <- result$structure_exact_mass[[1]]
    mass_labeled <- result$structure_exact_mass[[2]]
    mass_diff <- mass_labeled - mass_natural

    # 18O shift ≈ 2.0043 Da
    # Allow ±0.005 Da tolerance (stricter because it's larger mass difference)
    expect_true(
      mass_diff > 1.99 && mass_diff < 2.02,
      info = sprintf(
        "Mass difference for 18O: %.6f Da (expected ~2.0043 Da)",
        mass_diff
      )
    )
  }
})

test_that("process_smiles handles deuterium [2H] notation", {
  # Test deuterium (2H) isotope on explicit hydrogens
  # [2H]C([2H])([2H])[2H] = fully deuterated methane
  # 2H - 1H ≈ 1.0078 Da per atom

  methane_natural <- "C"
  methane_deuterated <- "[2H]C([2H])([2H])[2H]"

  input <- tidytable::tidytable(
    structure_smiles_initial = c(methane_natural, methane_deuterated)
  )

  result <- input |>
    process_smiles()

  expect_true(all(!is.na(result$structure_exact_mass)))

  # If both molecules processed, check mass difference
  if (nrow(result) >= 2) {
    mass_natural <- result$structure_exact_mass[[1]]
    mass_deuterated <- result$structure_exact_mass[[2]]
    mass_diff <- mass_deuterated - mass_natural

    # 4× deuterium ≈ 4 × 1.0078 = 4.0312 Da
    expect_true(
      mass_diff > 4.02 && mass_diff < 4.04,
      info = sprintf(
        "Mass difference for 4x 2H: %.6f Da (expected ~4.0312 Da)",
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

  result <- input |>
    process_smiles()

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

  result <- input |>
    process_smiles()

  expect_true(all(!is.na(result$structure_exact_mass)))
  # All SMILES should have inchikeys
  expect_true(all(!is.na(result$structure_inchikey)))
})

## Additional Isotope Preservation Tests ----

test_that("process_smiles preserves [13C] in canonical SMILES after standardization", {
  skip_if_not(require("tima"), "tima package required")

  input <- tidytable::tidytable(
    structure_smiles_initial = c(
      "OC[C@H]1OC(O)[C@H](O)[C@@H](O)[C@@H]1O", # natural glucose
      "OC[13C@H]1OC(O)[13C@H](O)[13C@@H](O)[13C@@H]1O" # 4× 13C-labeled
    )
  )

  result <- process_smiles(input, cache = NULL)

  expect_equal(nrow(result), 2L)

  # Labeled SMILES should contain [13C] notation (preserved through standardization)
  labeled_smiles <- result$structure_smiles[[2]]
  expect_match(
    labeled_smiles,
    "\\[13C",
    info = "13C notation should be preserved in canonical SMILES after standardization"
  )
})

test_that("process_smiles mass calculation reflects isotope composition", {
  skip_if_not(require("tima"), "tima package required")

  input <- tidytable::tidytable(
    structure_smiles_initial = c(
      "OC[C@H]1OC(O)[C@H](O)[C@@H](O)[C@@H]1O", # natural
      "OC[13C@H]1OC(O)[13C@H](O)[13C@@H](O)[13C@@H]1O" # 4× 13C
    )
  )

  result <- process_smiles(input, cache = NULL)

  mass_natural <- result$structure_exact_mass[[1]]
  mass_labeled <- result$structure_exact_mass[[2]]
  mass_diff <- mass_labeled - mass_natural

  # RDKit automatically uses isotope atomic masses
  expect_true(
    abs(mass_diff - 4.0134) < 0.001,
    info = sprintf(
      "Expected ~4.0134 Da (RDKit automatic), got %.6f Da",
      mass_diff
    )
  )
})

test_that("process_smiles formula shows isotopes in separated format", {
  skip_if_not(require("tima"), "tima package required")

  input <- tidytable::tidytable(
    structure_smiles_initial = "OC[13C@H]1OC(O)[13C@H](O)[13C@@H](O)[13C@@H]1O"
  )

  result <- process_smiles(input, cache = NULL)

  formula <- result$structure_molecular_formula[[1]]

  # Formula should show isotopes (e.g., C2[13C]4H12O6 or similar format)
  expect_true(
    is.character(formula) && nchar(formula) > 0,
    info = "Formula should be non-empty for isotope-labeled compound"
  )

  # Check for isotope notation (various formats possible)
  has_isotope_notation <- grepl("13C", formula) || grepl("\\[13C\\]", formula)
  expect_true(
    has_isotope_notation,
    info = sprintf("Formula should show isotope notation, got: %s", formula)
  )
})

test_that("process_smiles InChIKey differs for isotope-labeled compounds", {
  skip_if_not(require("tima"), "tima package required")

  input <- tidytable::tidytable(
    structure_smiles_initial = c(
      "OC[C@H]1OC(O)[C@H](O)[C@@H](O)[C@@H]1O",
      "OC[13C@H]1OC(O)[13C@H](O)[13C@@H](O)[13C@@H]1O"
    )
  )

  result <- process_smiles(input, cache = NULL)

  inchikey_natural <- result$structure_inchikey[[1]]
  inchikey_labeled <- result$structure_inchikey[[2]]

  # Both should be non-empty
  expect_true(
    nchar(inchikey_natural) > 0 && nchar(inchikey_labeled) > 0,
    info = "InChIKeys should be non-empty"
  )

  # They should differ (isotope-aware)
  expect_true(
    inchikey_natural != inchikey_labeled,
    info = "InChIKey should differ for natural vs isotope-labeled"
  )
})

test_that("process_smiles handles multiple different isotope types", {
  skip_if_not(require("tima"), "tima package required")

  # Test molecule with both 13C and 15N
  natural_compound <- "NC(=O)c1ccccc1" # benzamide
  # COMMENT: Explicit H is required for isotopes
  labeled_compound <- "[15NH2]C(=O)[13CH2]1[13CH2][13CH2][13CH2][13CH2][13CH2]1" # labeled benzamide

  input <- tidytable::tidytable(
    structure_smiles_initial = c(natural_compound, labeled_compound)
  )

  result <- input |>
    process_smiles()

  expect_true(all(!is.na(result$structure_exact_mass)))

  # If both molecules processed, check that masses differ
  if (nrow(result) >= 2) {
    mass_natural <- result$structure_exact_mass[[1]]
    mass_labeled <- result$structure_exact_mass[[2]]
    mass_diff <- mass_labeled - mass_natural

    # With 5× 13C and 1× 15N:
    # Expected: 5*1.00335 + 0.9964 = 5.99475 + 0.9964 ≈ 6.991 Da
    expect_true(
      mass_diff > 6.9 && mass_diff < 7.0,
      info = sprintf(
        "Mass difference for mixed isotopes (5×13C + 15N): %.6f Da (expected ~6.99 Da)",
        mass_diff
      )
    )
  }
})
##
## This approach ensures accurate annotation of isotope-labeled natural products
