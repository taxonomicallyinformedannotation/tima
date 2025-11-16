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

test_that("annotate_masses validates MS mode", {
  # Invalid MS mode should error before any file checks
  expect_error(
    annotate_masses(
      features = "ignored.tsv",
      library = "ignored.tsv",
      str_stereo = "ignored.tsv",
      str_met = "ignored.tsv",
      str_nam = "ignored.tsv",
      str_tax_cla = "ignored.tsv",
      str_tax_npc = "ignored.tsv",
      tolerance_ppm = 10,
      tolerance_rt = 0.02,
      ms_mode = "invalid"
    ),
    "pos.*neg"
  )

  expect_error(
    annotate_masses(
      features = "ignored.tsv",
      library = "ignored.tsv",
      str_stereo = "ignored.tsv",
      str_met = "ignored.tsv",
      str_nam = "ignored.tsv",
      str_tax_cla = "ignored.tsv",
      str_tax_npc = "ignored.tsv",
      tolerance_ppm = 10,
      tolerance_rt = 0.02,
      ms_mode = "positive"
    ),
    "pos.*neg"
  )
})

test_that("annotate_masses validates tolerance parameters", {
  # Negative tolerance should error (ppm)
  expect_error(
    annotate_masses(
      features = "ignored.tsv",
      library = "ignored.tsv",
      str_stereo = "ignored.tsv",
      str_met = "ignored.tsv",
      str_nam = "ignored.tsv",
      str_tax_cla = "ignored.tsv",
      str_tax_npc = "ignored.tsv",
      tolerance_ppm = -10,
      tolerance_rt = 0.02,
      ms_mode = "pos"
    ),
    "positive"
  )

  # Zero tolerance should error (ppm)
  expect_error(
    annotate_masses(
      features = "ignored.tsv",
      library = "ignored.tsv",
      str_stereo = "ignored.tsv",
      str_met = "ignored.tsv",
      str_nam = "ignored.tsv",
      str_tax_cla = "ignored.tsv",
      str_tax_npc = "ignored.tsv",
      tolerance_ppm = 0,
      tolerance_rt = 0.02,
      ms_mode = "pos"
    ),
    "positive"
  )

  # RT tolerance too high should error
  expect_error(
    annotate_masses(
      features = "ignored.tsv",
      library = "ignored.tsv",
      str_stereo = "ignored.tsv",
      str_met = "ignored.tsv",
      str_nam = "ignored.tsv",
      str_tax_cla = "ignored.tsv",
      str_tax_npc = "ignored.tsv",
      tolerance_ppm = 10,
      tolerance_rt = 0.2,
      ms_mode = "pos"
    ),
    "0.05"
  )
})

test_that("annotate_masses validates file existence", {
  skip("Integration test - requires file system setup")
})

# ==============================================================================
# Performance test kept skipped (manual)
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
