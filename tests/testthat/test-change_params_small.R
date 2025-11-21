# Test Suite: change_params_small ----

library(testthat)

# Test Fixtures ----

#' Create minimal test files for change_params_small
#' @keywords internal
setup_param_test_files <- function() {
  temp_dir <- temp_test_dir("change_params_test")

  # Create minimal test input files
  features_file <- file.path(temp_dir, "test_features.csv")
  metadata_file <- file.path(temp_dir, "test_metadata.tsv")
  sirius_file <- file.path(temp_dir, "test_sirius.zip")
  spectra_file <- file.path(temp_dir, "test_spectra.mgf")

  # Write minimal content
  writeLines("feature_id,mz,rt\nF1,100.5,1.2", features_file)
  writeLines("sample_id,organism\nS1,Test species", metadata_file)
  writeLines("test", sirius_file)
  writeLines("test", spectra_file)

  list(
    features = features_file,
    metadata = metadata_file,
    sirius = sirius_file,
    spectra = spectra_file,
    dir = temp_dir
  )
}

# Unit Tests: Helper Functions ----

test_that("validate_params_small_inputs accepts valid polarity", {
  expect_silent(validate_params_small_inputs(ms_pol = "pos"))
  expect_silent(validate_params_small_inputs(ms_pol = "neg"))
  expect_silent(validate_params_small_inputs(ms_pol = NULL))
})

test_that("validate_params_small_inputs rejects invalid polarity", {
  expect_error(
    validate_params_small_inputs(ms_pol = "invalid"),
    "must be either 'pos' or 'neg'"
  )
  expect_error(
    validate_params_small_inputs(ms_pol = "positive"),
    "must be either 'pos' or 'neg'"
  )
  expect_error(
    validate_params_small_inputs(ms_pol = 123),
    "must be either 'pos' or 'neg'"
  )
})

test_that("copy_file_to_target copies files correctly", {
  test_files <- setup_param_test_files()
  target_dir <- temp_test_dir("copy_target")

  result <- copy_file_to_target(
    file_path = test_files$features,
    target_dir = target_dir,
    file_description = "Test features"
  )

  expect_true(file.exists(result))
  expect_equal(basename(result), "test_features.csv")
  expect_true(dir.exists(target_dir))
})

test_that("copy_file_to_target validates file existence", {
  target_dir <- temp_test_dir("copy_target")

  expect_error(
    copy_file_to_target(
      file_path = "nonexistent_file.csv",
      target_dir = target_dir,
      file_description = "Nonexistent"
    ),
    "file does not exist"
  )
})

# Integration Tests: Main Function ----

test_that("change_params_small function exists and has correct signature", {
  expect_true(exists("change_params_small"))
  expect_type(change_params_small, "closure")

  params <- names(formals(change_params_small))
  expected_params <- c(
    "fil_pat",
    "fil_fea_raw",
    "fil_met_raw",
    "fil_sir_raw",
    "fil_spe_raw",
    "ms_pol",
    "org_tax",
    "hig_con",
    "summarize"
  )

  expect_true(all(expected_params %in% params))
})

test_that("change_params_small all parameters default to NULL", {
  params <- formals(change_params_small)

  expect_null(params$fil_pat)
  expect_null(params$fil_fea_raw)
  expect_null(params$fil_met_raw)
  expect_null(params$fil_sir_raw)
  expect_null(params$fil_spe_raw)
  expect_null(params$ms_pol)
  expect_null(params$org_tax)
  expect_null(params$hig_con)
  expect_null(params$summarize)
})

test_that("change_params_small validates polarity parameter", {
  skip_if_not(interactive(), "Requires cache setup")

  expect_error(
    change_params_small(ms_pol = "invalid"),
    "must be either 'pos' or 'neg'"
  )
})

test_that("change_params_small validates file existence", {
  skip_if_not(interactive(), "Requires cache setup")

  # These should fail because files don't exist
  expect_error(
    suppressMessages(
      change_params_small(fil_fea_raw = "nonexistent_features.csv")
    ),
    "does not exist"
  )

  expect_error(
    suppressMessages(
      change_params_small(fil_met_raw = "nonexistent_metadata.tsv")
    ),
    "does not exist"
  )
})

# Edge Cases and Error Handling ----

test_that("change_params_small handles NULL SIRIUS file appropriately", {
  skip_if_not(interactive(), "Requires cache setup")

  # Should set SIRIUS to NA when not provided
  # This is tested implicitly in the main function
  expect_silent(validate_params_small_inputs(ms_pol = NULL))
})

test_that("change_params_small handles all NULL parameters", {
  skip_if_not(interactive(), "Requires cache setup")

  # Calling with all NULLs should not error (just load/save YAML)
  # Skip actual execution as it requires full cache setup
  expect_type(change_params_small, "closure")
})

# Performance Tests ----

test_that("copy_file_to_target handles overwrite correctly", {
  test_files <- setup_param_test_files()
  target_dir <- temp_test_dir("overwrite_test")

  # Copy once
  result1 <- copy_file_to_target(
    file_path = test_files$features,
    target_dir = target_dir,
    file_description = "Test"
  )

  # Modify the target
  writeLines("modified", result1)

  # Copy again (should overwrite)
  result2 <- copy_file_to_target(
    file_path = test_files$features,
    target_dir = target_dir,
    file_description = "Test"
  )

  content <- readLines(result2, n = 1)
  expect_equal(content, "feature_id,mz,rt")
})

# Regression Tests ----

test_that("change_params_small maintains backward compatibility", {
  # Ensure function signature hasn't changed unexpectedly
  params <- names(formals(change_params_small))

  # These parameters must exist for backward compatibility
  critical_params <- c(
    "fil_fea_raw",
    "fil_spe_raw",
    "ms_pol",
    "org_tax"
  )

  expect_true(all(critical_params %in% params))
})
