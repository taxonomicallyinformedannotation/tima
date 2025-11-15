# ==============================================================================
# Test Suite: annotate_masses
# ==============================================================================

# Comprehensive Test Suite for annotate_masses
# Generated: 2025-11-14

library(testthat)
library(tima)

# ==============================================================================
# Test: Input Validation
# ==============================================================================

# test_that("annotate_masses validates MS mode", {
#   skip_on_cran()
#
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   # Setup minimal required files
#   get_file(
#     url = paths$urls$examples$features,
#     export = paths$data$source$features
#   )
#   prepare_features_tables()
#
#   fake_lotus(export = paths$data$source$libraries$sop$lotus)
#   fake_lotus(export = paths$data$source$libraries$sop$closed)
#   prepare_libraries_sop_lotus()
#   prepare_libraries_sop_closed()
#   prepare_libraries_sop_merged()
#
#   # Invalid MS mode should error - function checks for 'pos' or 'neg' only
#   expect_error(
#     annotate_masses(
#       tolerance_ppm = 10,
#       tolerance_rt = 0.02,
#       ms_mode = "invalid"
#     ),
#     "pos.*neg"
#   )
#
#   expect_error(
#     annotate_masses(
#       tolerance_ppm = 10,
#       tolerance_rt = 0.02,
#       ms_mode = "positive"
#     ),
#     "pos.*neg"
#   )
# })

# test_that("annotate_masses validates tolerance parameters", {
#   skip_on_cran()
#
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$features,
#     export = paths$data$source$features
#   )
#   prepare_features_tables()
#
#   fake_lotus(export = paths$data$source$libraries$sop$lotus)
#   fake_lotus(export = paths$data$source$libraries$sop$closed)
#   prepare_libraries_sop_lotus()
#   prepare_libraries_sop_closed()
#   prepare_libraries_sop_merged()
#
#   # Negative tolerance should error
#   expect_error(
#     annotate_masses(
#       tolerance_ppm = -10,
#       tolerance_rt = 0.02,
#       ms_mode = "pos"
#     ),
#     "positive"
#   )
#
#   # Zero tolerance should error
#   expect_error(
#     annotate_masses(
#       tolerance_ppm = 0,
#       tolerance_rt = 0.02,
#       ms_mode = "pos"
#     ),
#     "positive"
#   )
# })

test_that("annotate_masses validates file existence", {
  skip_on_cran()

  local_test_project(copy = TRUE)

  # Missing features file should error
  expect_error(
    annotate_masses(
      features = "nonexistent_features.tsv",
      tolerance_ppm = 10,
      tolerance_rt = 0.02,
      ms_mode = "pos"
    ),
    "not found|does not exist"
  )
})

# test_that("annotate_masses validates monocharged adducts are available", {
#   skip_on_cran()
#
#   # This test ensures the safety check for monocharged adducts works
#   # When adduct/cluster lists are empty or contain only multicharged species,
#   # the function should fail with a clear error message
#
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   # Setup minimal required files
#   get_file(
#     url = paths$urls$examples$features,
#     export = paths$data$source$features
#   )
#   prepare_features_tables()
#
#   fake_lotus(export = paths$data$source$libraries$sop$lotus)
#   fake_lotus(export = paths$data$source$libraries$sop$closed)
#   prepare_libraries_sop_lotus()
#   prepare_libraries_sop_closed()
#   prepare_libraries_sop_merged()
#
#   # Empty adducts list should trigger safety check
#   expect_error(
#     annotate_masses(
#       tolerance_ppm = 10,
#       tolerance_rt = 0.02,
#       ms_mode = "pos",
#       adducts_list = list(pos = character(0), neg = c("[M-H]-")),
#       clusters_list = list(pos = character(0), neg = character(0))
#     ),
#     "No monocharged adducts|No valid monocharged"
#   )
# })

# ==============================================================================
# Test: Positive Mode Annotation
# ==============================================================================

# test_that("annotate_masses works in positive mode", {
#   skip_on_cran()
#
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   # Setup complete workflow
#   get_file(
#     url = paths$urls$examples$features,
#     export = paths$data$source$features
#   )
#   prepare_features_tables()
#
#   fake_lotus(export = paths$data$source$libraries$sop$lotus)
#   fake_lotus(export = paths$data$source$libraries$sop$closed)
#   prepare_libraries_sop_lotus()
#   prepare_libraries_sop_closed()
#   prepare_libraries_sop_merged()
#
#   # Should complete without error
#   expect_no_error(
#     annotate_masses(
#       tolerance_ppm = 10,
#       tolerance_rt = 0.02,
#       ms_mode = "pos"
#     )
#   )
#
#   # Output files should exist
#   expect_true(file.exists("data/interim/annotations/example_features_ms1.tsv.gz"))
# })

# ==============================================================================
# Test: Negative Mode Annotation
# ==============================================================================

# test_that("annotate_masses works in negative mode", {
#   skip_on_cran()
#
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$features,
#     export = paths$data$source$features
#   )
#   prepare_features_tables()
#
#   fake_lotus(export = paths$data$source$libraries$sop$lotus)
#   fake_lotus(export = paths$data$source$libraries$sop$closed)
#   prepare_libraries_sop_lotus()
#   prepare_libraries_sop_closed()
#   prepare_libraries_sop_merged()
#
#   expect_no_error(
#     annotate_masses(
#       tolerance_ppm = 10,
#       tolerance_rt = 0.02,
#       ms_mode = "neg"
#     )
#   )
# })

# ==============================================================================
# Test: Different Tolerance Settings
# ==============================================================================

# test_that("annotate_masses accepts strict tolerances", {
#   skip_on_cran()
#
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$features,
#     export = paths$data$source$features
#   )
#   prepare_features_tables()
#
#   fake_lotus(export = paths$data$source$libraries$sop$lotus)
#   fake_lotus(export = paths$data$source$libraries$sop$closed)
#   prepare_libraries_sop_lotus()
#   prepare_libraries_sop_closed()
#   prepare_libraries_sop_merged()
#
#   # Very strict tolerances
#   expect_no_error(
#     annotate_masses(
#       tolerance_ppm = 1,
#       tolerance_rt = 0.01,
#       ms_mode = "pos"
#     )
#   )
# })

# test_that("annotate_masses accepts high but valid tolerances", {
#   skip_on_cran()
#
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$features,
#     export = paths$data$source$features
#   )
#   prepare_features_tables()
#
#   fake_lotus(export = paths$data$source$libraries$sop$lotus)
#   fake_lotus(export = paths$data$source$libraries$sop$closed)
#   prepare_libraries_sop_lotus()
#   prepare_libraries_sop_closed()
#   prepare_libraries_sop_merged()
#
#   # High tolerance at upper limits (20 ppm, 0.05 min)
#   expect_no_error(
#     annotate_masses(
#       tolerance_ppm = 20,
#       tolerance_rt = 0.05,
#       ms_mode = "pos"
#     )
#   )
# })

# ==============================================================================
# Test: Empty/Edge Cases
# ==============================================================================

# test_that("annotate_masses handles empty features table", {
#   skip_on_cran()
#
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   # Create empty features file
#   tidytable::tidytable(
#     feature_id = character(0),
#     rt = numeric(0),
#     mz = numeric(0)
#   ) |>
#     tidytable::fwrite("data/source/empty_features.csv")
#
#   prepare_features_tables(
#     features = "data/source/empty_features.csv",
#     output = "data/interim/features/empty_features.tsv.gz"
#   )
#
#   fake_lotus(export = paths$data$source$libraries$sop$lotus)
#   fake_lotus(export = paths$data$source$libraries$sop$closed)
#   prepare_libraries_sop_lotus()
#   prepare_libraries_sop_closed()
#   prepare_libraries_sop_merged()
#
#   # Should handle gracefully
#   expect_no_error(
#     annotate_masses(
#       features = "data/interim/features/empty_features.tsv.gz",
#       tolerance_ppm = 10,
#       tolerance_rt = 0.02,
#       ms_mode = "pos"
#     )
#   )
# })

# test_that("annotate_masses handles features without RT", {
#   skip_on_cran()
#
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   # Create features without RT column
#   tidytable::tidytable(
#     "row ID" = 1:3,
#     "row m/z" = c(123.4567, 234.5678, 345.6789),
#     "sample.mzML Peak area" = c(10000, 20000, 30000)
#   ) |>
#     tidytable::fwrite("data/source/features_no_rt.csv")
#
#   prepare_features_tables(
#     features = "data/source/features_no_rt.csv",
#     output = "data/interim/features/features_no_rt.tsv.gz"
#   )
#
#   fake_lotus(export = paths$data$source$libraries$sop$lotus)
#   fake_lotus(export = paths$data$source$libraries$sop$closed)
#   prepare_libraries_sop_lotus()
#   prepare_libraries_sop_closed()
#   prepare_libraries_sop_merged()
#
#   expect_no_error(
#     annotate_masses(
#       features = "data/interim/features/features_no_rt.tsv.gz",
#       tolerance_ppm = 10,
#       tolerance_rt = 0.02,
#       ms_mode = "pos"
#     )
#   )
# })

# ==============================================================================
# Test: Custom Adduct Lists
# ==============================================================================

# test_that("annotate_masses accepts custom adduct lists", {
#   skip_on_cran()
#
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$features,
#     export = paths$data$source$features
#   )
#   prepare_features_tables()
#
#   fake_lotus(export = paths$data$source$libraries$sop$lotus)
#   fake_lotus(export = paths$data$source$libraries$sop$closed)
#   prepare_libraries_sop_lotus()
#   prepare_libraries_sop_closed()
#   prepare_libraries_sop_merged()
#
#   # Custom adduct list
#   custom_adducts <- list(
#     pos = c("[M+H]+", "[M+Na]+"),
#     neg = c("[M-H]-")
#   )
#
#   expect_no_error(
#     annotate_masses(
#       tolerance_ppm = 10,
#       tolerance_rt = 0.02,
#       ms_mode = "pos",
#       adducts_list = custom_adducts
#     )
#   )
# })

# ==============================================================================
# Test: Output Validation
# ==============================================================================

# test_that("annotate_masses produces valid output structure", {
#   skip_on_cran()
#
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$features,
#     export = paths$data$source$features
#   )
#   prepare_features_tables()
#
#   fake_lotus(export = paths$data$source$libraries$sop$lotus)
#   fake_lotus(export = paths$data$source$libraries$sop$closed)
#   prepare_libraries_sop_lotus()
#   prepare_libraries_sop_closed()
#   prepare_libraries_sop_merged()
#
#   annotate_masses(
#     tolerance_ppm = 10,
#     tolerance_rt = 0.02,
#     ms_mode = "pos"
#   )
#
#   # Check output file exists and is valid
#   output_file <- "data/interim/annotations/example_features_ms1.tsv.gz"
#   expect_true(file.exists(output_file))
#
#   # Load and validate structure
#   result <- tidytable::fread(output_file)
#   expect_s3_class(result, "data.frame")
#
#   # Expected columns
#   expected_cols <- c(
#     "feature_id",
#     "candidate_structure_inchikey_connectivity_layer"
#   )
#
#   for (col in expected_cols) {
#     expect_true(col %in% colnames(result),
#       info = paste("Missing column:", col)
#     )
#   }
# })

# ==============================================================================
# Test: Performance & Scalability
# ==============================================================================

test_that("annotate_masses completes in reasonable time", {
  skip_on_cran()
  skip("Performance test - run manually")

  local_test_project(copy = TRUE)
  paths <- get_default_paths()

  get_file(
    url = paths$urls$examples$features,
    export = paths$data$source$features
  )
  prepare_features_tables()

  fake_lotus(export = paths$data$source$libraries$sop$lotus)
  fake_lotus(export = paths$data$source$libraries$sop$closed)
  prepare_libraries_sop_lotus()
  prepare_libraries_sop_closed()
  prepare_libraries_sop_merged()

  start_time <- Sys.time()

  annotate_masses(
    tolerance_ppm = 10,
    tolerance_rt = 0.02,
    ms_mode = "pos"
  )

  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")

  # Should complete reasonably fast for example data
  expect_true(elapsed < 60, info = paste("Took", elapsed, "seconds"))
})
