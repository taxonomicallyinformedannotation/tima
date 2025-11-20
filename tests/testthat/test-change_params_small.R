# Test Suite: change_params_small ----

library(testthat)

test_that("change_params_small updates all parameters correctly", {
  skip_on_cran()
  paths <- local_test_project(copy = TRUE)

  # Create temporary test files
  temp_features <- tempfile(fileext = ".csv")
  temp_metadata <- tempfile(fileext = ".tsv")
  temp_spectra <- tempfile(fileext = ".mgf")
  temp_sirius <- tempfile(fileext = ".zip")

  writeLines("test", temp_features)
  writeLines("test", temp_metadata)
  writeLines("test", temp_spectra)
  writeLines("test", temp_sirius)

  expect_no_error(
    change_params_small(
      fil_pat = "test_pattern",
      fil_fea_raw = temp_features,
      fil_met_raw = temp_metadata,
      fil_sir_raw = temp_sirius,
      fil_spe_raw = temp_spectra,
      ms_pol = "pos",
      org_tax = "Gentiana lutea",
      hig_con = TRUE,
      summarize = FALSE
    )
  )

  # Verify YAML was written
  expect_true(file.exists(paths$params$prepare_params))
})

test_that("change_params_small handles minimal parameters (all NULL)", {
  skip_on_cran()
  paths <- local_test_project(copy = TRUE)

  # Test with all NULL parameters
  expect_no_error(
    change_params_small()
  )

  expect_true(file.exists(paths$params$prepare_params))
})

test_that("change_params_small handles individual parameters", {
  skip_on_cran()

  # Test fil_pat only
  paths <- local_test_project(copy = TRUE)
  expect_no_error(change_params_small(fil_pat = "test_pattern"))

  # Test ms_pol only
  paths <- local_test_project(copy = TRUE)
  expect_no_error(change_params_small(ms_pol = "neg"))

  # Test org_tax only
  paths <- local_test_project(copy = TRUE)
  expect_no_error(change_params_small(org_tax = "Homo sapiens"))

  # Test hig_con only
  paths <- local_test_project(copy = TRUE)
  expect_no_error(change_params_small(hig_con = FALSE))

  # Test summarize only
  paths <- local_test_project(copy = TRUE)
  expect_no_error(change_params_small(summarize = TRUE))
})

test_that("change_params_small validates file existence", {
  skip_on_cran()
  paths <- local_test_project(copy = TRUE)

  # Non-existent features file
  expect_error(
    change_params_small(fil_fea_raw = "nonexistent.csv"),
    "Your features' file does not exist"
  )

  # Non-existent metadata file
  expect_error(
    change_params_small(fil_met_raw = "nonexistent.tsv"),
    "Your metadata file does not exist"
  )

  # Non-existent sirius directory
  expect_error(
    change_params_small(fil_sir_raw = "nonexistent.zip"),
    "Your sirius directory does not exist"
  )

  # Non-existent spectra file
  expect_error(
    change_params_small(fil_spe_raw = "nonexistent.mgf"),
    "Your spectra file does not exist"
  )
})

test_that("change_params_small copies files to correct locations", {
  skip_on_cran()
  paths <- local_test_project(copy = TRUE)

  temp_features <- tempfile(fileext = ".csv")
  writeLines("features_content", temp_features)

  change_params_small(fil_fea_raw = temp_features)

  # Check file was copied to data/source
  copied_file <- file.path(paths$data$source$path, basename(temp_features))
  expect_true(file.exists(copied_file))
  expect_equal(readLines(copied_file), "features_content")
})

# test_that("change_params_small handles NULL sirius file correctly", {
#   skip_on_cran()
#   paths <- local_test_project(copy = TRUE)
#
#   # When fil_sir_raw is NULL, it should be set to NA in YAML
#   change_params_small(fil_sir_raw = NULL)
#
#   yaml_content <- yaml::read_yaml(paths$params$prepare_params)
#   expect_true(is.na(yaml_content$files$annotations$raw$sirius) ||
#     yaml_content$files$annotations$raw$sirius == "null")
# })

# test_that("change_params_small handles NULL org_tax correctly", {
#   skip_on_cran()
#   paths <- local_test_project(copy = TRUE)
#
#   # When org_tax is NULL, it should be set to NA in YAML
#   change_params_small(org_tax = NULL)
#
#   yaml_content <- yaml::read_yaml(paths$params$prepare_params)
#   expect_true(is.na(yaml_content$organisms$taxon) ||
#     yaml_content$organisms$taxon == "null")
# })

test_that("change_params_small overwrites existing files", {
  skip_on_cran()
  paths <- local_test_project(copy = TRUE)

  temp_file <- tempfile(fileext = ".csv")
  writeLines("version1", temp_file)

  change_params_small(fil_fea_raw = temp_file)

  # Overwrite with new content
  writeLines("version2", temp_file)
  change_params_small(fil_fea_raw = temp_file)

  copied_file <- file.path(paths$data$source$path, basename(temp_file))
  expect_equal(readLines(copied_file), "version2")
})
