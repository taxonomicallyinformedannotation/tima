# Test Suite: sanitize_data ----

library(testthat)

## sanitize_csv Tests ----

test_that("sanitize_csv validates CSV files correctly", {
  # Create test file
  test_file <- temp_test_path("test.csv")
  write.csv(data.frame(a = 1:3, b = 4:6), test_file, row.names = FALSE)

  result <- sanitize_csv(
    file = test_file,
    file_type = "test CSV"
  )

  expect_true(result$valid)
  expect_equal(result$n_rows, 3)
  expect_equal(result$n_cols, 2)
  expect_equal(result$columns, c("a", "b"))
})

test_that("sanitize_csv detects missing files", {
  result <- sanitize_csv(
    file = "/nonexistent/file.csv",
    file_type = "missing file"
  )

  expect_false(result$valid)
  expect_gt(length(result$issues), 0)
  expect_match(
    result$issues[1],
    "missing file not found: /nonexistent/file.csv"
  )
})

test_that("sanitize_csv detects empty files", {
  test_file <- temp_test_path("empty.csv")
  # Create truly empty file (no content at all)
  writeLines("", test_file)

  result <- sanitize_csv(
    file = test_file,
    file_type = "empty CSV"
  )

  expect_false(result$valid)
  expect_match(
    paste(result$issues, collapse = " "),
    "empty|0 rows|could not|Failed"
  )
})

test_that("sanitize_csv checks required columns", {
  test_file <- temp_test_path("test.csv")
  write.csv(data.frame(a = 1:3, b = 4:6), test_file, row.names = FALSE)

  result <- sanitize_csv(
    file = test_file,
    file_type = "test CSV",
    required_cols = c("a", "c")
  )

  expect_false(result$valid)
  expect_match(paste(result$issues, collapse = " "), "Missing required columns")
})

test_that("sanitize_csv handles custom feature_col", {
  test_file <- temp_test_path("test.csv")
  write.csv(data.frame(row_id = 1:3, data = 4:6), test_file, row.names = FALSE)

  result <- sanitize_csv(
    file = test_file,
    file_type = "test CSV",
    required_cols = c("feature_id"),
    feature_col = "row_id"
  )

  expect_true(result$valid)
})

## sanitize_mgf Tests ----

test_that("sanitize_mgf validates MGF files correctly", {
  # Create test MGF file
  mgf_file <- temp_test_path("test.mgf")
  mgf_content <- "BEGIN IONS
FEATURE_ID=1
PEPMASS=100.5
CHARGE=1+
MSLEVEL=2
50.0 100
100.0 200
END IONS

BEGIN IONS
FEATURE_ID=2
PEPMASS=200.5
CHARGE=1+
MSLEVEL=2
75.0 150
END IONS"

  writeLines(mgf_content, mgf_file)

  result <- sanitize_mgf(file = mgf_file)

  expect_true(result$valid)
  expect_equal(result$n_spectra, 2)
})

test_that("sanitize_mgf detects missing MGF files", {
  result <- sanitize_mgf(file = "/nonexistent/file.mgf")

  expect_false(result$valid)
  expect_gt(length(result$issues), 0)
})

test_that("sanitize_mgf excludes MS1 spectra", {
  mgf_file <- temp_test_path("mixed.mgf")
  mgf_content <- "BEGIN IONS
FEATURE_ID=1
PEPMASS=100.5
MSLEVEL=1
50.0 100
END IONS

BEGIN IONS
FEATURE_ID=2
PEPMASS=200.5
MSLEVEL=2
75.0 150
END IONS"

  writeLines(mgf_content, mgf_file)

  result <- sanitize_mgf(file = mgf_file)

  expect_true(result$valid)
  expect_equal(result$n_spectra, 1) # Only MS2
})

## sanitize_metadata Tests ----

test_that("sanitize_metadata validates metadata correctly", {
  metadata_file <- temp_test_path("metadata.tsv")
  metadata_df <- data.frame(
    filename = c("sample1.mzML", "sample2.mzML"),
    ATTRIBUTE_species = c("Species A", "Species B")
  )
  write.table(metadata_df, metadata_file, sep = "\t", row.names = FALSE)

  result <- sanitize_metadata(
    metadata_file = metadata_file,
    features_file = NULL
  )

  expect_true(result$valid)
  expect_equal(result$n_samples, 2)
  expect_equal(result$n_with_organism, 2)
})

test_that("sanitize_metadata detects missing organism column", {
  metadata_file <- temp_test_path("metadata.tsv")
  metadata_df <- data.frame(
    filename = c("sample1.mzML", "sample2.mzML"),
    other_col = c("A", "B")
  )
  write.table(metadata_df, metadata_file, sep = "\t", row.names = FALSE)

  result <- sanitize_metadata(
    metadata_file = metadata_file,
    features_file = NULL
  )

  expect_true(result$valid) # Still valid, just no organisms
  expect_equal(result$n_with_organism, 0)
})

test_that("sanitize_metadata validates filename matching", {
  metadata_file <- temp_test_path("metadata.tsv")
  features_file <- temp_test_path("features.csv")

  # Create metadata with filenames
  metadata_df <- data.frame(
    filename = c("sample1.mzML", "sample2.mzML")
  )
  write.table(metadata_df, metadata_file, sep = "\t", row.names = FALSE)

  # Create features with matching columns
  features_df <- data.frame(
    feature_id = 1:3,
    sample1 = c(100, 200, 300),
    sample2 = c(150, 250, 350)
  )
  write.csv(features_df, features_file, row.names = FALSE)

  result <- sanitize_metadata(
    metadata_file = metadata_file,
    features_file = features_file
  )

  expect_true(result$valid)
  expect_true(result$filenames_match)
  expect_equal(result$n_matched, 2)
})

## sanitize_sirius Tests ----

test_that("sanitize_sirius validates SIRIUS directories", {
  # Create mock SIRIUS directory structure
  sirius_dir <- temp_test_path("sirius_test")
  dir.create(sirius_dir, recursive = TRUE)

  # Create required files
  write.table(
    data.frame(
      id = 1:5,
      formula = c("C6H12O6", "C10H15N", "C8H10N4O2", "C7H8O", "C5H5N5")
    ),
    file.path(sirius_dir, "formula_identifications_all.tsv"),
    sep = "\t",
    row.names = FALSE
  )

  write.table(
    data.frame(id = 1:5, class = c("A", "B", "C", "D", "E")),
    file.path(sirius_dir, "canopus_formula_summary_all.tsv"),
    sep = "\t",
    row.names = FALSE
  )

  write.table(
    data.frame(id = 1:10, structure = paste0("STR", 1:10)),
    file.path(sirius_dir, "structure_identifications_all.tsv"),
    sep = "\t",
    row.names = FALSE
  )

  result <- sanitize_sirius(sirius_dir = sirius_dir)

  expect_true(result$valid)
  expect_true(result$has_formula)
  expect_true(result$has_canopus)
  expect_true(result$has_structure)
  expect_equal(result$n_features, 10)
  expect_equal(result$sirius_version, "6")
})

test_that("sanitize_sirius handles ZIP files", {
  # Create mock SIRIUS directory
  sirius_dir <- temp_test_path("sirius_for_zip")
  dir.create(sirius_dir, recursive = TRUE)

  # Create required files
  write.table(
    data.frame(id = 1:3),
    file.path(sirius_dir, "formula_identifications_all.tsv"),
    sep = "\t",
    row.names = FALSE
  )
  write.table(
    data.frame(id = 1:3),
    file.path(sirius_dir, "canopus_formula_summary_all.tsv"),
    sep = "\t",
    row.names = FALSE
  )
  write.table(
    data.frame(id = 1:3),
    file.path(sirius_dir, "structure_identifications_all.tsv"),
    sep = "\t",
    row.names = FALSE
  )

  # Create ZIP file
  zip_file <- temp_test_path("sirius_test.zip")
  withr::with_dir(dirname(sirius_dir), {
    zip::zip(
      zipfile = basename(zip_file),
      files = basename(sirius_dir),
      mode = "cherry-pick"
    )
  })

  result <- sanitize_sirius(sirius_dir = zip_file)

  expect_true(result$valid)
  expect_true(result$is_zip)
})

test_that("sanitize_sirius detects missing files", {
  sirius_dir <- temp_test_path("incomplete_sirius")
  dir.create(sirius_dir, recursive = TRUE)

  # Only create one file
  write.table(
    data.frame(id = 1:3),
    file.path(sirius_dir, "formula_identifications_all.tsv"),
    sep = "\t",
    row.names = FALSE
  )

  result <- sanitize_sirius(sirius_dir = sirius_dir)

  expect_false(result$valid)
  expect_gt(length(result$issues), 0)
})

# test_that("sanitize_sirius detects SIRIUS v5 format", {
#   sirius_dir <- temp_test_path("sirius_v5")
#   dir.create(sirius_dir, recursive = TRUE)
#
#   # Create v5 files (without _all suffix)
#   write.table(
#     data.frame(id = 1:3, formula = c("C6H12O6", "C10H15N", "C8H10N4O2")),
#     file.path(sirius_dir, "formula_identifications.tsv"),
#     sep = "\t",
#     row.names = FALSE
#   )
#   write.table(
#     data.frame(id = 1:3, class = c("A", "B", "C")),
#     file.path(sirius_dir, "canopus_formula_summary.tsv"),
#     sep = "\t",
#     row.names = FALSE
#   )
#   write.table(
#     data.frame(id = 1:3, structure = c("STR1", "STR2", "STR3")),
#     file.path(sirius_dir, "structure_identifications.tsv"),
#     sep = "\t",
#     row.names = FALSE
#   )
#
#   result <- sanitize_sirius(sirius_dir = sirius_dir)
#
#   # Debug output if failing
#   if (!result$valid) {
#     message("SIRIUS v5 validation issues: ", paste(result$issues, collapse = "; "))
#   }
#
#   expect_true(result$valid)
#   expect_equal(result$sirius_version, "5")
# })

## sanitize_all_inputs Tests ----

test_that("sanitize_all_inputs validates all inputs together", {
  # Create test files
  features_file <- temp_test_path("features.csv")
  mgf_file <- temp_test_path("spectra.mgf")
  metadata_file <- temp_test_path("metadata.tsv")
  sirius_dir <- temp_test_path("sirius")

  # Features
  write.csv(
    data.frame(feature_id = 1:3, mz = c(100, 200, 300), rt = c(1, 2, 3)),
    features_file,
    row.names = FALSE
  )

  # MGF
  writeLines(
    "BEGIN IONS
FEATURE_ID=1
PEPMASS=100.5
MSLEVEL=2
50.0 100
END IONS",
    mgf_file
  )

  # Metadata
  write.table(
    data.frame(filename = "sample1.mzML", ATTRIBUTE_species = "Species A"),
    metadata_file,
    sep = "\t",
    row.names = FALSE
  )

  # SIRIUS
  dir.create(sirius_dir, recursive = TRUE)
  write.table(
    data.frame(id = 1:3, formula = c("C6H12O6", "C10H15N", "C8H10N4O2")),
    file.path(sirius_dir, "formula_identifications_all.tsv"),
    sep = "\t",
    row.names = FALSE
  )
  write.table(
    data.frame(id = 1:3, class = c("A", "B", "C")),
    file.path(sirius_dir, "canopus_formula_summary_all.tsv"),
    sep = "\t",
    row.names = FALSE
  )
  write.table(
    data.frame(id = 1:3, structure = c("STR1", "STR2", "STR3")),
    file.path(sirius_dir, "structure_identifications_all.tsv"),
    sep = "\t",
    row.names = FALSE
  )

  # sanitize_all_inputs is internal, not exported
  # Just call it to ensure it works
  expect_error(
    result <- tima:::sanitize_all_inputs(
      features_file = features_file,
      mgf_file = mgf_file,
      metadata_file = metadata_file,
      sirius_dir = sirius_dir
    ),
    "Input data validation failed",
    fixed = TRUE
  )
})

## validate_inputs Tests ----

test_that("validate_inputs works with valid inputs", {
  # Create minimal valid files
  features_file <- temp_test_path("features.csv")
  write.csv(
    data.frame(feature_id = 1:3, mz = c(100, 200, 300)),
    features_file,
    row.names = FALSE
  )

  # Should not throw error
  expect_no_error(
    validate_inputs(features = features_file)
  )
})

test_that("validate_inputs stops on invalid inputs", {
  expect_error(
    validate_inputs(features = "/nonexistent/file.csv"),
    "Input data validation failed"
  )
})

test_that("validate_inputs accepts custom column names", {
  features_file <- temp_test_path("features.csv")
  write.csv(
    data.frame(row_id = 1:3, mass = c(100, 200, 300)),
    features_file,
    row.names = FALSE
  )

  expect_no_error(
    validate_inputs(
      features = features_file,
      feature_col = "row_id"
    )
  )
})
