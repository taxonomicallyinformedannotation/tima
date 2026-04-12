# Test Suite: select_sirius_columns ----

library(testthat)

## Validation ----

test_that("test-select_sirius_columns_canopus validates df parameter", {
  expect_error(
    select_sirius_columns_canopus(df = "not_a_df", sirius_version = "5"),
    "Fix: Ensure input is a valid data frame",
    fixed = TRUE
  )
})

test_that("test-select_sirius_columns_canopus validates sirius_version", {
  df <- tidytable::tidytable(id = "test")

  expect_error(
    select_sirius_columns_canopus(df = df, sirius_version = "7"),
    "sirius_version must be '5' or '6'",
    class = "tima_validation_error"
  )

  expect_error(
    select_sirius_columns_canopus(df = df, sirius_version = "invalid"),
    "sirius_version must be '5' or '6'",
    class = "tima_validation_error"
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
  ## exact_mass is now recomputed from SMILES downstream; only error_mz is kept
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

test_that("test-select_sirius_columns_formulas supports massErrorPrecursor(ppm) alias", {
  df <- tidytable::tidytable(
    mappingFeatureId = c("F1"),
    ionMass = c("180.0634"),
    `massErrorPrecursor(ppm)` = c("2.5"),
    adduct = c("[M+H]+")
  )

  result <- select_sirius_columns_formulas(df = df, sirius_version = "6")

  ## exact_mass is now recomputed from SMILES downstream; only error_mz is kept
  expect_true("candidate_structure_error_mz" %in% names(result))
  expect_true(!is.na(result$candidate_structure_error_mz[[1L]]))
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

test_that("test-select_sirius_columns_canopus supports hash-style alias columns", {
  df <- tidytable::tidytable(
    mappingFeatureId = c("F_alias"),
    adduct = c("[M+H]+"),
    molecularFormula = c("C10H14N2"),
    `NPC#pathway` = c("Alkaloids"),
    `NPC#pathway Probability` = c("0.91"),
    `NPC#superclass` = c("Pathway superclass"),
    `NPC#superclass Probability` = c("0.82"),
    `NPC#class` = c("Pathway class"),
    `NPC#class Probability` = c("0.77"),
    `ClassyFire#superclass` = c("Organic compounds"),
    `ClassyFire#superclass probability` = c("0.95"),
    `ClassyFire#class` = c("Alkaloids and derivatives"),
    `ClassyFire#class Probability` = c("0.87"),
    `ClassyFire#most specific class` = c("Indole alkaloids"),
    `ClassyFire#most specific class Probability` = c("0.81")
  )

  result <- select_sirius_columns_canopus(df = df, sirius_version = "6")

  expect_equal(result$feature_id[[1L]], "F_alias")
  expect_equal(result$feature_pred_tax_npc_01pat_val[[1L]], "Alkaloids")
  expect_equal(
    result$feature_pred_tax_cla_04dirpar_val[[1L]],
    "Indole alkaloids"
  )
})

test_that("test-select_sirius_columns_spectral maps direct spectral hits", {
  df <- tidytable::tidytable(
    mappingFeatureId = "F_spec",
    analogHit = "false",
    similarity = "0.81",
    sharedPeaks = "20",
    referenceSplash = "splash10-aaaa",
    referenceAdduct = "[M+H]+",
    referenceSmiles = "CCO",
    referenceName = "SpecMatch",
    InChIkey2D = "ABCDEFGHIJKLMN",
    ionMass = "201.0500",
    referencePrecursorMz = "201.0488"
  )

  result <- select_sirius_columns_spectral(df = df, sirius_version = "6")

  expect_equal(result$feature_id[[1L]], "F_spec")
  expect_equal(result$candidate_library[[1L]], "SIRIUS spectral")
  expect_equal(result$candidate_spectrum_id[[1L]], "splash10-aaaa")
  expect_equal(result$candidate_structure_name[[1L]], "SpecMatch")
  expect_equal(as.numeric(result$candidate_score_similarity[[1L]]), 0.81)
  expect_equal(
    as.integer(result$candidate_count_similarity_peaks_matched[[1L]]),
    20L
  )
  expect_equal(
    round(as.numeric(result$candidate_structure_error_mz[[1L]]), 4),
    round(201.0500 - 201.0488, 4)
  )
})

test_that("test-select_sirius_columns_spectral does not fallback for spectrum id and name", {
  df <- tidytable::tidytable(
    mappingFeatureId = "F_spec3",
    analogHit = "false",
    similarity = "0.66",
    sharedPeaks = "8",
    splash = "splash10-bbbb",
    name = "FallbackName",
    referenceAdduct = "[M+H]+",
    referenceSmiles = "CCN",
    InChIkey2D = "YYYYYYYYYYYYYY",
    ionMass = "155.1000",
    referencePrecursorMz = "155.0990"
  )

  result <- select_sirius_columns_spectral(df = df, sirius_version = "6")

  expect_true(is.na(result$candidate_spectrum_id[[1L]]))
  expect_true(is.na(result$candidate_structure_name[[1L]]))
})

test_that("test-select_sirius_columns_spectral approximates error_mz from precursor mz columns", {
  df <- tidytable::tidytable(
    mappingFeatureId = "F_spec2",
    analogHit = "false",
    similarity = "0.75",
    sharedPeaks = "10",
    referenceAdduct = "[M+H]+",
    referenceSmiles = "CCCO",
    InChIkey2D = "XXXXXXXXXXX",
    ionMass = "201.0500",
    referencePrecursorMz = "201.0488"
  )

  result <- select_sirius_columns_spectral(df = df, sirius_version = "6")

  err <- as.numeric(result$candidate_structure_error_mz[[1L]])
  expect_true(!is.na(err))
  expect_equal(round(err, 4), round(201.0500 - 201.0488, 4))
})

test_that("test-select_sirius_columns_spectral maps analog hits", {
  df <- tidytable::tidytable(
    mappingFeatureId = "F_analog",
    analogHit = "true",
    similarity = "0.37",
    sharedPeaks = "7",
    referenceAdduct = "[M+H3N+H]+",
    referenceSmiles = "CCCC",
    referenceName = "AnalogMatch",
    InChIkey2D = "ZZZZZZZZZZZZZZ",
    ionMass = "300.1500",
    referencePrecursorMz = "283.1184"
  )

  result <- select_sirius_columns_spectral(df = df, sirius_version = "6")

  expect_equal(result$feature_id[[1L]], "F_analog")
  expect_equal(result$candidate_library[[1L]], "SIRIUS spectral (analog)")
  expect_equal(as.numeric(result$candidate_score_similarity[[1L]]), 0.37)
  # For analog hits the mass deviation is the delta-mass between query and reference
  expect_equal(
    round(as.numeric(result$candidate_structure_error_mz[[1L]]), 4),
    round(300.1500 - 283.1184, 4)
  )
})
