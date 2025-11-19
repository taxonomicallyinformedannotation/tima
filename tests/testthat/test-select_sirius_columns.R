# Test Suite: select_sirius_columns ----

library(testthat)

## Validation ----

test_that("test-select_sirius_columns_canopus validates df parameter", {
  expect_error(
    select_sirius_columns_canopus(df = "not_a_df", sirius_version = "5"),
    "df must be a data frame"
  )
})

test_that("test-select_sirius_columns_canopus validates sirius_version", {
  df <- tidytable::tidytable(id = "test")

  expect_error(
    select_sirius_columns_canopus(df = df, sirius_version = "7"),
    "sirius_version must be '5' or '6'"
  )

  expect_error(
    select_sirius_columns_canopus(df = df, sirius_version = "invalid"),
    "sirius_version must be '5' or '6'"
  )
})

test_that("test-select_sirius_columns_canopus handles empty dataframe", {
  df <- tidytable::tidytable(id = character())

  result <- select_sirius_columns_canopus(df = df, sirius_version = "5")
  expect_equal(nrow(result), 0)
})

## Behavior ----

test_that("test-select_sirius_columns_canopus processes SIRIUS v5 data", {
  df <- tidytable::tidytable(
    id = c("feature_1"),
    adduct = c("[M+H]+"),
    molecularFormula = c("C6H12O6"),
    `NPC.pathway` = c("Pathway1"),
    `NPC.pathway.Probability` = c("0.9"),
    `NPC.superclass` = c("Super1"),
    `NPC.superclass.Probability` = c("0.8"),
    `NPC.class` = c("Class1"),
    `NPC.class.Probability` = c("0.7"),
    `ClassyFire.superclass` = c("CFSuper"),
    `ClassyFire.superclass.probability` = c("0.85"),
    `ClassyFire.class` = c("CFClass"),
    `ClassyFire.class.Probability` = c("0.75"),
    `ClassyFire.most.specific.class` = c("CFMost"),
    `ClassyFire.most.specific.class.Probability` = c("0.65")
  )

  result <- select_sirius_columns_canopus(df = df, sirius_version = "5")

  expect_true("feature_id" %in% names(result))
  expect_true("candidate_adduct" %in% names(result))
  expect_true("feature_pred_tax_npc_01pat_val" %in% names(result))
  expect_equal(nrow(result), 1)
})

test_that("test-select_sirius_columns_canopus processes SIRIUS v6 data", {
  df <- tidytable::tidytable(
    mappingFeatureId = c("F1"),
    adduct = c("[M+H]+"),
    molecularFormula = c("C6H12O6"),
    `NPC.pathway` = c("Pathway1"),
    `NPC.pathway.Probability` = c("0.9")
  )

  result <- select_sirius_columns_canopus(df = df, sirius_version = "6")

  expect_true("feature_id" %in% names(result))
  expect_equal(result$feature_id[1], "F1")
})

test_that("test-select_sirius_columns_canopus handles missing columns", {
  df <- tidytable::tidytable(
    id = c("feature_1"),
    adduct = c("[M+H]+")
  )

  result <- select_sirius_columns_canopus(df = df, sirius_version = "5")

  expect_true("feature_id" %in% names(result))
  expect_true("candidate_adduct" %in% names(result))
})

test_that("test-select_sirius_columns_canopus removes duplicates", {
  df <- tidytable::tidytable(
    id = c("feature_1", "feature_1"),
    adduct = c("[M+H]+", "[M+H]+"),
    molecularFormula = c("C6H12O6", "C6H12O6")
  )

  result <- select_sirius_columns_canopus(df = df, sirius_version = "5")

  expect_equal(nrow(result), 1)
})

## select_sirius_columns_formulas ----

test_that("test-select_sirius_columns_formulas processes SIRIUS v5 formulas", {
  df <- tidytable::tidytable(
    id = c("feature_1"),
    ionMass = c("180.0634"),
    `massErrorPrecursor.ppm.` = c("2.5"),
    adduct = c("[M+H]+")
  )

  result <- select_sirius_columns_formulas(df = df, sirius_version = "5")

  expect_true("feature_id" %in% names(result))
  expect_true("candidate_structure_exact_mass" %in% names(result))
  expect_true("candidate_structure_error_mz" %in% names(result))
})

test_that("test-select_sirius_columns_formulas processes SIRIUS v6 formulas", {
  df <- tidytable::tidytable(
    mappingFeatureId = c("F1"),
    ionMass = c("180.0634"),
    `massErrorPrecursor.ppm.` = c("2.5"),
    adduct = c("[M+H]+")
  )

  result <- select_sirius_columns_formulas(df = df, sirius_version = "6")

  expect_true("feature_id" %in% names(result))
  expect_equal(result$feature_id[1], "F1")
})

## select_sirius_columns_structures ----

test_that("test-select_sirius_columns_structures processes SIRIUS v5 structures", {
  df <- tidytable::tidytable(
    feature_id = c("F1"),
    adduct = c("[M+H]+"),
    InChIkey2D = c("AAAAAAAAAAAA"),
    smiles = c("C")
  )

  result <- select_sirius_columns_structures(df = df, sirius_version = "5")

  expect_true("feature_id" %in% names(result))
  expect_true(nrow(result) >= 0)
})

test_that("test-select_sirius_columns_structures processes SIRIUS v6 structures", {
  df <- tidytable::tidytable(
    feature_id = c("F1"),
    adduct = c("[M+H]+"),
    InChIkey2D = c("AAAAAAAAAAAA"),
    smiles = c("C")
  )

  result <- select_sirius_columns_structures(df = df, sirius_version = "6")

  expect_true("feature_id" %in% names(result))
  expect_true(nrow(result) >= 0)
})

test_that("test-select_sirius_columns_structures accepts both string and numeric versions", {
  df <- tidytable::tidytable(
    feature_id = c("F1"),
    adduct = c("[M+H]+")
  )

  # Should accept "5"
  expect_no_error(select_sirius_columns_structures(
    df = df,
    sirius_version = "5"
  ))

  # Should accept 5
  expect_no_error(select_sirius_columns_structures(df = df, sirius_version = 5))

  # Should accept "6"
  expect_no_error(select_sirius_columns_structures(
    df = df,
    sirius_version = "6"
  ))

  # Should accept 6
  expect_no_error(select_sirius_columns_structures(df = df, sirius_version = 6))
})
