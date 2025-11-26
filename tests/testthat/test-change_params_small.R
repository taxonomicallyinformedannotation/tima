# Test Suite: change_params_small ----

library(testthat)

## Setup ----

# Create temporary test files
create_temp_test_files <- function(base_dir = tempdir()) {
  files <- list(
    features = file.path(base_dir, "features.csv"),
    metadata = file.path(base_dir, "metadata.tsv"),
    sirius = file.path(base_dir, "sirius.zip"),
    spectra = file.path(base_dir, "spectra.mgf")
  )

  # Create dummy files
  write.csv(data.frame(a = 1:3), files$features, row.names = FALSE)
  write.table(
    data.frame(b = 4:6),
    files$metadata,
    sep = "\t",
    row.names = FALSE
  )
  writeLines("dummy_sirius", files$sirius)
  writeLines("dummy_mgf", files$spectra)

  files
}

## Input Validation Tests ----

test_that("change_params_small validates ms_pol parameter", {
  expect_error(
    change_params_small(ms_pol = "invalid"),
    "must be either 'pos' or 'neg'"
  )

  expect_error(
    change_params_small(ms_pol = "positive"),
    "must be either 'pos' or 'neg'"
  )

  expect_error(
    change_params_small(ms_pol = 123),
    "must be either 'pos' or 'neg'"
  )
})

test_that("change_params_small handles NULL ms_pol", {
  # Should not error with NULL
  expect_no_error(
    change_params_small(ms_pol = NULL)
  )
})

## File Validation Tests ----

test_that("change_params_small errors on missing features file", {
  expect_error(
    change_params_small(fil_fea_raw = "nonexistent_features.csv"),
    "Features file does not exist"
  )
})

test_that("change_params_small errors on missing metadata file", {
  expect_error(
    change_params_small(fil_met_raw = "nonexistent_metadata.tsv"),
    "Metadata file does not exist"
  )
})

test_that("change_params_small errors on missing SIRIUS file", {
  expect_error(
    change_params_small(fil_sir_raw = "nonexistent_sirius.zip"),
    "SIRIUS annotations file does not exist"
  )
})

test_that("change_params_small errors on missing spectra file", {
  expect_error(
    change_params_small(fil_spe_raw = "nonexistent_spectra.mgf"),
    "Spectra file does not exist"
  )
})

## Successful Update Tests ----

test_that("change_params_small updates file pattern", {
  # Setup
  test_project <- local_test_project()

  # Execute
  change_params_small(fil_pat = "test_pattern")

  # Verify
  yaml_data <- load_yaml_files()
  yaml_small <- yaml_data$yamls_params$prepare_params

  expect_equal(yaml_small$files$pattern, "test_pattern")
})

test_that("change_params_small updates MS polarity", {
  test_project <- local_test_project()

  change_params_small(ms_pol = "pos")

  yaml_data <- load_yaml_files()
  yaml_small <- yaml_data$yamls_params$prepare_params

  expect_equal(yaml_small$ms$polarity, "pos")
})

test_that("change_params_small handles both pos and neg polarity", {
  test_project <- local_test_project()

  # Test positive
  change_params_small(ms_pol = "pos")
  yaml_data <- load_yaml_files()
  expect_equal(yaml_data$yamls_params$prepare_params$ms$polarity, "pos")

  # Test negative
  change_params_small(ms_pol = "neg")
  yaml_data <- load_yaml_files()
  expect_equal(yaml_data$yamls_params$prepare_params$ms$polarity, "neg")
})

test_that("change_params_small updates organism taxonomy", {
  test_project <- local_test_project()

  change_params_small(org_tax = "Gentiana lutea")

  yaml_data <- load_yaml_files()
  yaml_small <- yaml_data$yamls_params$prepare_params

  expect_equal(yaml_small$organisms$taxon, "Gentiana lutea")
})

test_that("change_params_small updates high_confidence flag", {
  test_project <- local_test_project()

  change_params_small(hig_con = TRUE)

  yaml_data <- load_yaml_files()
  yaml_small <- yaml_data$yamls_params$prepare_params

  expect_true(yaml_small$options$high_confidence)
})

test_that("change_params_small updates summarize flag", {
  test_project <- local_test_project()

  change_params_small(summarize = FALSE)

  yaml_data <- load_yaml_files()
  yaml_small <- yaml_data$yamls_params$prepare_params

  expect_false(yaml_small$options$summarize)
})

## File Copy Tests ----

test_that("change_params_small copies features file", {
  skip_on_cran()

  test_project <- local_test_project()
  test_files <- create_temp_test_files()

  change_params_small(fil_fea_raw = test_files$features)

  # Check file was copied to source directory
  paths <- get_default_paths()
  target_file <- file.path(
    paths$data$source$path,
    basename(test_files$features)
  )

  expect_true(file.exists(target_file))
})

test_that("change_params_small copies metadata file", {
  skip_on_cran()

  test_project <- local_test_project()
  test_files <- create_temp_test_files()

  change_params_small(fil_met_raw = test_files$metadata)

  paths <- get_default_paths()
  target_file <- file.path(
    paths$data$source$path,
    basename(test_files$metadata)
  )

  expect_true(file.exists(target_file))
})

test_that("change_params_small copies spectra file", {
  skip_on_cran()

  test_project <- local_test_project()
  test_files <- create_temp_test_files()

  change_params_small(fil_spe_raw = test_files$spectra)

  paths <- get_default_paths()
  target_file <- file.path(paths$data$source$path, basename(test_files$spectra))

  expect_true(file.exists(target_file))
})

test_that("change_params_small copies SIRIUS file to correct directory", {
  skip_on_cran()

  test_project <- local_test_project()
  test_files <- create_temp_test_files()

  change_params_small(fil_sir_raw = test_files$sirius)

  paths <- get_default_paths()
  target_file <- file.path(
    paths$data$interim$annotations$path,
    basename(test_files$sirius)
  )

  expect_true(file.exists(target_file))
})

## Multiple Parameters Tests ----

test_that("change_params_small handles multiple parameters simultaneously", {
  skip_on_cran()

  test_project <- local_test_project()
  test_files <- create_temp_test_files()

  change_params_small(
    fil_pat = "multi_test",
    fil_fea_raw = test_files$features,
    fil_met_raw = test_files$metadata,
    ms_pol = "pos",
    org_tax = "Arabidopsis thaliana",
    hig_con = TRUE,
    summarize = FALSE
  )

  yaml_data <- load_yaml_files()
  yaml_small <- yaml_data$yamls_params$prepare_params

  expect_equal(yaml_small$files$pattern, "multi_test")
  expect_equal(yaml_small$ms$polarity, "pos")
  expect_equal(yaml_small$organisms$taxon, "Arabidopsis thaliana")
  expect_true(yaml_small$options$high_confidence)
  expect_false(yaml_small$options$summarize)
})

## NA Handling Tests ----

# test_that("change_params_small sets organism to NA when NULL", {
#   test_project <- local_test_project()
#
#   change_params_small(org_tax = NULL)
#
#   yaml_data <- load_yaml_files()
#   yaml_small <- yaml_data$yamls_params$prepare_params
#
#   expect_true(is.na(yaml_small$organisms$taxon))
# })

# test_that("change_params_small sets SIRIUS to NA when NULL", {
#   test_project <- local_test_project()
#
#   change_params_small(fil_sir_raw = NULL)
#
#   yaml_data <- load_yaml_files()
#   yaml_small <- yaml_data$yamls_params$prepare_params
#
#   expect_true(is.na(yaml_small$files$annotations$raw$sirius))
# })

## Return Value Tests ----

test_that("change_params_small returns invisible NULL", {
  test_project <- local_test_project()

  result <- withVisible(change_params_small(fil_pat = "test"))

  expect_null(result$value)
  expect_false(result$visible)
})

## Integration Tests ----

test_that("change_params_small creates directories if missing", {
  skip_on_cran()

  test_project <- local_test_project()

  # Remove directories to ensure they're created
  paths <- get_default_paths()
  unlink(paths$data$source$path, recursive = TRUE)
  unlink(paths$data$interim$annotations$path, recursive = TRUE)

  test_files <- create_temp_test_files()

  change_params_small(fil_fea_raw = test_files$features)

  expect_true(dir.exists(paths$data$source$path))
})

test_that("change_params_small preserves existing YAML structure", {
  test_project <- local_test_project()

  # Load initial structure
  yaml_before <- load_yaml_files()
  initial_structure <- names(yaml_before$yamls_params$prepare_params)

  # Make changes
  change_params_small(fil_pat = "test", ms_pol = "pos")

  # Load after changes
  yaml_after <- load_yaml_files()
  final_structure <- names(yaml_after$yamls_params$prepare_params)

  # Structure should remain the same
  expect_setequal(initial_structure, final_structure)
})

## Helper Function Tests ----

test_that("validate_params_small_inputs works correctly", {
  # Valid inputs
  expect_silent(validate_params_small_inputs("pos"))
  expect_silent(validate_params_small_inputs("neg"))
  expect_silent(validate_params_small_inputs(NULL))

  # Invalid inputs
  expect_error(validate_params_small_inputs("invalid"), "must be either")
})

test_that("create_yaml_null_handler converts NA correctly", {
  handler <- create_yaml_null_handler()

  # Test NA conversion
  result_na <- handler(NA)
  expect_s3_class(result_na, "verbatim")
  expect_equal(as.character(result_na), "null")

  # Test non-NA pass-through
  result_val <- handler("value")
  expect_equal(result_val, "value")

  result_num <- handler(123)
  expect_equal(result_num, 123)
})

## Edge Cases ----

test_that("change_params_small handles all NULL parameters", {
  test_project <- local_test_project()

  # Should complete without error
  expect_no_error(
    change_params_small(
      fil_pat = NULL,
      fil_fea_raw = NULL,
      fil_met_raw = NULL,
      fil_sir_raw = NULL,
      fil_spe_raw = NULL,
      ms_pol = NULL,
      org_tax = NULL,
      hig_con = NULL,
      summarize = NULL
    )
  )
})

test_that("change_params_small handles file overwriting", {
  skip_on_cran()

  test_project <- local_test_project()
  test_files <- create_temp_test_files()

  # Copy file twice
  change_params_small(fil_fea_raw = test_files$features)

  # Modify original
  write.csv(data.frame(a = 10:13), test_files$features, row.names = FALSE)

  # Copy again - should overwrite
  change_params_small(fil_fea_raw = test_files$features)

  paths <- get_default_paths()
  target_file <- file.path(
    paths$data$source$path,
    basename(test_files$features)
  )

  # Should contain updated data
  copied_data <- read.csv(target_file)
  expect_true(10 %in% copied_data$a)
})
