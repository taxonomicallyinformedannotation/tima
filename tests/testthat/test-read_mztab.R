# Test Suite: read_mztab ----

library(testthat)

## Internal Helpers ----

get_mztab_fixture <- function() {
  resolve_fixture_path("minimal.mztab")
}

## .parse_mztab_adduct ----

test_that(".parse_mztab_adduct converts mzTab adduct to TIMA format", {
  # Standard conversion: remove charge number

  expect_equal(tima:::.parse_mztab_adduct("[M+H]1+"), "[M+H]+")
  expect_equal(tima:::.parse_mztab_adduct("[M-H]1-"), "[M-H]-")
  expect_equal(tima:::.parse_mztab_adduct("[M+2H]2+"), "[M+2H]+")
  # Already in TIMA format
  expect_equal(tima:::.parse_mztab_adduct("[M+H]+"), "[M+H]+")
  # NA handling
  expect_true(is.na(tima:::.parse_mztab_adduct(NA_character_)))
  expect_true(is.na(tima:::.parse_mztab_adduct("null")))
  expect_true(is.na(tima:::.parse_mztab_adduct("")))
  # Vector input
  result <- tima:::.parse_mztab_adduct(c("[M+H]1+", "[M-H]1-", NA))
  expect_equal(result[1], "[M+H]+")
  expect_equal(result[2], "[M-H]-")
  expect_true(is.na(result[3]))
})

## .extract_charge_from_adduct ----

test_that(".extract_charge_from_adduct extracts charge correctly", {
  expect_equal(tima:::.extract_charge_from_adduct("[M+H]+"), "1+")
  expect_equal(tima:::.extract_charge_from_adduct("[M-H]-"), "1-")
  expect_equal(tima:::.extract_charge_from_adduct("[M+2H]2+"), "2+")
  expect_equal(tima:::.extract_charge_from_adduct(NA), "1+")
  expect_equal(tima:::.extract_charge_from_adduct(""), "1+")
})

## .map_mztab_sml_to_features ----

test_that(".map_mztab_sml_to_features maps columns correctly", {
  sml <- data.frame(
    SML_ID = c("1", "2"),
    exp_mass_to_charge = c("303.05", "285.04"),
    retention_time_in_seconds = c("180", "210"),
    adduct_ions = c("[M+H]1+", "[M-H]1-"),
    chemical_name = c("Quercetin", "Kaempferol"),
    abundance_assay.1. = c("10000", "5000"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  # Fix column name to match mzTab format
  names(sml)[6] <- "abundance_assay[1]"

  result <- tima:::.map_mztab_sml_to_features(sml)

  expect_true("feature_id" %in% names(result))
  expect_true("mz" %in% names(result))
  expect_true("rt" %in% names(result))
  expect_true("adduct" %in% names(result))

  expect_equal(result$feature_id, c("1", "2"))
  expect_equal(result$mz, c(303.05, 285.04))
  # RT should be converted from seconds to minutes

  expect_equal(result$rt, c(3.0, 3.5))
  # Adduct should have charge number removed
  expect_equal(result$adduct, c("[M+H]+", "[M-H]-"))
})

## .validate_mztab_structure ----

test_that(".validate_mztab_structure rejects invalid objects", {
  expect_error(
    tima:::.validate_mztab_structure(list(Small_Molecule = NULL)),
    regexp = "SML|Small_Molecule|structure"
  )
  expect_error(
    tima:::.validate_mztab_structure(list()),
    regexp = "SML|Small_Molecule|structure"
  )
})

## .create_ms1_mgf_from_features ----

test_that(".create_ms1_mgf_from_features creates valid MGF", {
  features <- data.frame(
    feature_id = c("F1", "F2"),
    mz = c(303.05, 285.04),
    rt = c(3.0, 3.5),
    adduct = c("[M+H]+", "[M-H]-"),
    stringsAsFactors = FALSE
  )
  out <- temp_test_path("ms1_test.mgf")
  result <- tima:::.create_ms1_mgf_from_features(features, out)

  expect_equal(result, out)
  expect_true(file.exists(out))

  content <- readLines(out)
  expect_true(any(grepl("BEGIN IONS", content)))
  expect_true(any(grepl("END IONS", content)))
  expect_true(any(grepl("PEPMASS=303.05", content)))
  expect_true(any(grepl("TITLE=F1", content)))
})

test_that(".create_ms1_mgf_from_features returns NULL for missing columns", {
  features <- data.frame(feature_id = "F1", stringsAsFactors = FALSE)
  out <- temp_test_path("ms1_missing.mgf")
  result <- tima:::.create_ms1_mgf_from_features(features, out)
  expect_null(result)
})

test_that(".create_ms1_mgf_from_features handles NA m/z values", {
  features <- data.frame(
    feature_id = c("F1", "F2"),
    mz = c(NA, 100.0),
    stringsAsFactors = FALSE
  )
  out <- temp_test_path("ms1_na_mz.mgf")
  result <- tima:::.create_ms1_mgf_from_features(features, out)

  expect_equal(result, out)
  content <- readLines(out)
  # Only F2 should be in the output
  expect_true(any(grepl("TITLE=F2", content)))
  expect_false(any(grepl("TITLE=F1", content)))
})

## .convert_sme_to_mgf ----

test_that(".convert_sme_to_mgf returns NULL for empty data", {
  result <- tima:::.convert_sme_to_mgf(NULL, NULL, "out.mgf")
  expect_null(result)

  result <- tima:::.convert_sme_to_mgf(
    data.frame(),
    data.frame(),
    "out.mgf"
  )
  expect_null(result)
})

test_that(".convert_sme_to_mgf creates MGF from SME data", {
  sme <- data.frame(
    SME_ID = c("1", "2"),
    exp_mass_to_charge = c("303.05", "285.04"),
    retention_time_in_seconds = c("180", "210"),
    charge = c("1", "1"),
    stringsAsFactors = FALSE
  )
  sml <- data.frame(SML_ID = c("1", "2"), stringsAsFactors = FALSE)
  out <- temp_test_path("sme_test.mgf")

  result <- tima:::.convert_sme_to_mgf(sme, sml, out)

  expect_equal(result, out)
  expect_true(file.exists(out))
  content <- readLines(out)
  expect_true(any(grepl("BEGIN IONS", content)))
  expect_true(any(grepl("PEPMASS=303.05", content)))
  expect_true(any(grepl("MSLEVEL=2", content)))
})

## Full read_mztab integration ----

test_that("read_mztab reads a minimal mzTab-M file", {
  mztab_file <- get_mztab_fixture()
  out_features <- temp_test_path("mztab_features.csv")
  out_spectra <- temp_test_path("mztab_spectra.mgf")

  result <- read_mztab(
    input = mztab_file,
    output_features = out_features,
    output_spectra = out_spectra
  )

  expect_true(file.exists(result$features))
  features <- tidytable::fread(result$features)

  # Should have at least the SML entries
  expect_true(nrow(features) >= 2)
  expect_true("feature_id" %in% names(features))
  expect_true("mz" %in% names(features))
})

test_that("read_mztab validates input file existence", {
  expect_error(
    read_mztab(
      input = "nonexistent_file.mztab",
      output_features = temp_test_path("features.csv")
    ),
    regexp = "not found"
  )
})

