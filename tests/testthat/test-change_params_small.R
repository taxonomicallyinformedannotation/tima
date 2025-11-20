# Test Suite: change_params_small ----

library(testthat)

test_that("change_params_small validates feature file exists", {
  paths <- local_test_project(copy = TRUE)

  expect_error(
    change_params_small(fil_fea_raw = "nonexistent_features.csv"),
    "Your features' file does not exist"
  )
})

test_that("change_params_small validates metadata file exists", {
  paths <- local_test_project(copy = TRUE)

  expect_error(
    change_params_small(fil_met_raw = "nonexistent_metadata.tsv"),
    "Your metadata file does not exist"
  )
})

test_that("change_params_small validates sirius directory exists", {
  paths <- local_test_project(copy = TRUE)

  expect_error(
    change_params_small(fil_sir_raw = "nonexistent_sirius.zip"),
    "Your sirius directory does not exist"
  )
})

test_that("change_params_small validates spectra file exists", {
  paths <- local_test_project(copy = TRUE)

  expect_error(
    change_params_small(fil_spe_raw = "nonexistent_spectra.mgf"),
    "Your spectra file does not exist"
  )
})

test_that("change_params_small handles minimal parameters (all NULL)", {
  paths <- local_test_project(copy = TRUE)

  # Test with all NULL parameters - should complete without error
  expect_silent(change_params_small())

  # Verify YAML was written
  expect_true(file.exists(paths$params$prepare_params))
})

test_that("change_params_small updates pattern parameter", {
  paths <- local_test_project(copy = TRUE)

  change_params_small(fil_pat = "test_pattern_123")

  # Read YAML and verify
  yaml_content <- yaml::read_yaml(paths$params$prepare_params)
  expect_equal(yaml_content$files$pattern, "test_pattern_123")
})

test_that("change_params_small updates polarity parameter", {
  paths <- local_test_project(copy = TRUE)

  change_params_small(ms_pol = "neg")

  yaml_content <- yaml::read_yaml(paths$params$prepare_params)
  expect_equal(yaml_content$ms$polarity, "neg")
})

test_that("change_params_small updates organism taxon parameter", {
  paths <- local_test_project(copy = TRUE)

  change_params_small(org_tax = "Arabidopsis thaliana")

  yaml_content <- yaml::read_yaml(paths$params$prepare_params)
  expect_equal(yaml_content$organisms$taxon, "Arabidopsis thaliana")
})

test_that("change_params_small sets taxon to NULL when NULL", {
  paths <- local_test_project(copy = TRUE)

  change_params_small(org_tax = NULL)

  yaml_content <- yaml::read_yaml(paths$params$prepare_params)
  expect_true(
    is.na(yaml_content$organisms$taxon) ||
      is.null(yaml_content$organisms$taxon)
  )
})

test_that("change_params_small updates high_confidence parameter", {
  paths <- local_test_project(copy = TRUE)

  change_params_small(hig_con = FALSE)

  yaml_content <- yaml::read_yaml(paths$params$prepare_params)
  expect_equal(yaml_content$options$high_confidence, FALSE)
})

test_that("change_params_small updates summarize parameter", {
  paths <- local_test_project(copy = TRUE)

  change_params_small(summarize = TRUE)

  yaml_content <- yaml::read_yaml(paths$params$prepare_params)
  expect_equal(yaml_content$options$summarize, TRUE)
})

test_that("change_params_small copies and updates feature file", {
  paths <- local_test_project(copy = TRUE)

  temp_features <- make_tmp_file("features", fileext = ".csv")
  writeLines("test_features", temp_features)

  change_params_small(fil_fea_raw = temp_features)

  yaml_content <- yaml::read_yaml(paths$params$prepare_params)

  # File should be copied to data source
  expect_true(file.exists(yaml_content$files$features$raw))
  expect_true(grepl("data/source", yaml_content$files$features$raw))
})

test_that("change_params_small copies and updates metadata file", {
  paths <- local_test_project(copy = TRUE)

  temp_metadata <- make_tmp_file("metadata", fileext = ".tsv")
  writeLines("test_metadata", temp_metadata)

  change_params_small(fil_met_raw = temp_metadata)

  yaml_content <- yaml::read_yaml(paths$params$prepare_params)

  expect_true(file.exists(yaml_content$files$metadata$raw))
  expect_true(grepl("data/source", yaml_content$files$metadata$raw))
})

test_that("change_params_small copies and updates spectra file", {
  paths <- local_test_project(copy = TRUE)

  temp_spectra <- make_tmp_file("spectra", fileext = ".mgf")
  writeLines("test_spectra", temp_spectra)

  change_params_small(fil_spe_raw = temp_spectra)

  yaml_content <- yaml::read_yaml(paths$params$prepare_params)

  expect_true(file.exists(yaml_content$files$spectral$raw))
  expect_true(grepl("data/source", yaml_content$files$spectral$raw))
})

test_that("change_params_small copies and updates sirius directory", {
  paths <- local_test_project(copy = TRUE)

  temp_sirius <- make_tmp_file("sirius", fileext = ".zip")
  writeLines("test_sirius", temp_sirius)

  change_params_small(fil_sir_raw = temp_sirius)

  yaml_content <- yaml::read_yaml(paths$params$prepare_params)

  expect_true(file.exists(yaml_content$files$annotations$raw$sirius))
  expect_true(grepl(
    "data/interim/annotations",
    yaml_content$files$annotations$raw$sirius
  ))
})

test_that("change_params_small sets sirius to NULL when NULL", {
  paths <- local_test_project(copy = TRUE)

  change_params_small(fil_sir_raw = NULL)

  yaml_content <- yaml::read_yaml(paths$params$prepare_params)

  expect_true(is.null(yaml_content$files$annotations$raw$sirius))
})

test_that("change_params_small updates all parameters correctly", {
  paths <- local_test_project(copy = TRUE)

  # Create temporary test files
  temp_features <- make_tmp_file("features", fileext = ".csv")
  temp_metadata <- make_tmp_file("metadata", fileext = ".tsv")
  temp_spectra <- make_tmp_file("spectra", fileext = ".mgf")
  temp_sirius <- make_tmp_file("sirius", fileext = ".zip")

  writeLines("test", temp_features)
  writeLines("test", temp_metadata)
  writeLines("test", temp_spectra)
  writeLines("test", temp_sirius)

  expect_silent(
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

  # Verify all parameters were set
  yaml_content <- yaml::read_yaml(paths$params$prepare_params)
  expect_equal(yaml_content$files$pattern, "test_pattern")
  expect_equal(yaml_content$ms$polarity, "pos")
  expect_equal(yaml_content$organisms$taxon, "Gentiana lutea")
  expect_equal(yaml_content$options$high_confidence, TRUE)
  expect_equal(yaml_content$options$summarize, FALSE)
})
#   expect_no_error(change_params_small(fil_pat = "test_pattern"))
#
#   # Test ms_pol only
#   paths <- local_test_project(copy = TRUE)
#   expect_no_error(change_params_small(ms_pol = "neg"))
#
#   # Test org_tax only
#   paths <- local_test_project(copy = TRUE)
#   expect_no_error(change_params_small(org_tax = "Homo sapiens"))
#
#   # Test hig_con only
#   paths <- local_test_project(copy = TRUE)
#   expect_no_error(change_params_small(hig_con = FALSE))
#
#   # Test summarize only
#   paths <- local_test_project(copy = TRUE)
#   expect_no_error(change_params_small(summarize = TRUE))
# })

# test_that("change_params_small validates file existence", {
#   paths <- local_test_project(copy = TRUE)
#
#   # Non-existent features file
#   expect_error(
#     change_params_small(fil_fea_raw = "nonexistent.csv"),
#     "Your features' file does not exist"
#   )
#
#   # Non-existent metadata file
#   expect_error(
#     change_params_small(fil_met_raw = "nonexistent.tsv"),
#     "Your metadata file does not exist"
#   )
#
#   # Non-existent sirius directory
#   expect_error(
#     change_params_small(fil_sir_raw = "nonexistent.zip"),
#     "Your sirius directory does not exist"
#   )
#
#   # Non-existent spectra file
#   expect_error(
#     change_params_small(fil_spe_raw = "nonexistent.mgf"),
#     "Your spectra file does not exist"
#   )
# })

# test_that("change_params_small copies files to correct locations", {
#   paths <- local_test_project(copy = TRUE)
#
#   temp_features <- tempfile(fileext = ".csv")
#   writeLines("features_content", temp_features)
#
#   change_params_small(fil_fea_raw = temp_features)
#
#   # Check file was copied to data/source
#   copied_file <- file.path(paths$data$source$path, basename(temp_features))
#   expect_true(file.exists(copied_file))
#   expect_equal(readLines(copied_file), "features_content")
# })

# test_that("change_params_small handles NULL sirius file correctly", {
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
#   paths <- local_test_project(copy = TRUE)
#
#   # When org_tax is NULL, it should be set to NA in YAML
#   change_params_small(org_tax = NULL)
#
#   yaml_content <- yaml::read_yaml(paths$params$prepare_params)
#   expect_true(is.na(yaml_content$organisms$taxon) ||
#     yaml_content$organisms$taxon == "null")
# })

# test_that("change_params_small overwrites existing files", {
#   paths <- local_test_project(copy = TRUE)
#
#   temp_file <- tempfile(fileext = ".csv")
#   writeLines("version1", temp_file)
#
#   change_params_small(fil_fea_raw = temp_file)
#
#   # Overwrite with new content
#   writeLines("version2", temp_file)
#   change_params_small(fil_fea_raw = temp_file)
#
#   copied_file <- file.path(paths$data$source$path, basename(temp_file))
#   expect_equal(readLines(copied_file), "version2")
# })
