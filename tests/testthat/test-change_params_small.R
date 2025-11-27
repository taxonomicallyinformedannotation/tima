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
  skip_on_ci()
  # Should not error with NULL
  expect_no_error(
    change_params_small(ms_pol = NULL)
  )
})

## File Validation Tests ----

test_that("change_params_small errors on missing features file", {
  skip_on_ci()
  expect_error(
    change_params_small(fil_fea_raw = "nonexistent_features.csv"),
    "Features file does not exist"
  )
})

test_that("change_params_small errors on missing metadata file", {
  skip_on_ci()
  expect_error(
    change_params_small(fil_met_raw = "nonexistent_metadata.tsv"),
    "Metadata file does not exist"
  )
})

test_that("change_params_small errors on missing SIRIUS file", {
  skip_on_ci()
  expect_error(
    change_params_small(fil_sir_raw = "nonexistent_sirius.zip"),
    "SIRIUS annotations file does not exist"
  )
})

test_that("change_params_small errors on missing spectra file", {
  skip_on_ci()
  expect_error(
    change_params_small(fil_spe_raw = "nonexistent_spectra.mgf"),
    "Spectra file does not exist"
  )
})

## Successful Update Tests ----

test_that("change_params_small runs with valid polarity", {
  skip_on_ci()
  skip_if_not_installed("yaml")

  # Create minimal environment
  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  dir.create(file.path(tmpdir, "params"), recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  # Create minimal params files
  minimal_params <- list(
    ms = list(polarity = "pos"),
    files = list(
      pattern = "test",
      features = list(raw = "features.csv"),
      metadata = list(raw = "metadata.tsv"),
      annotations = list(raw = list(sirius = "sirius.zip")),
      spectral = list(raw = "spectra.mgf")
    )
  )

  yaml::write_yaml(
    minimal_params,
    file.path(tmpdir, "params/prepare_params.yaml")
  )
  yaml::write_yaml(
    minimal_params,
    file.path(tmpdir, "params/prepare_params_advanced.yaml")
  )

  # Create dummy files in source directory
  source_dir <- file.path(tmpdir, "data/source")
  dir.create(source_dir, recursive = TRUE)

  write.csv(
    data.frame(a = 1),
    file.path(source_dir, "features.csv"),
    row.names = FALSE
  )
  write.table(
    data.frame(b = 1),
    file.path(source_dir, "metadata.tsv"),
    sep = "\t",
    row.names = FALSE
  )
  writeLines("dummy", file.path(source_dir, "sirius.zip"))
  writeLines("dummy", file.path(source_dir, "spectra.mgf"))

  # Should run without error using cache_dir parameter
  result <- change_params_small(ms_pol = "neg", cache_dir = tmpdir)

  expect_null(result) # Returns invisible NULL

  # Check that params were updated
  params <- yaml::read_yaml(file.path(tmpdir, "params/prepare_params.yaml"))
  expect_equal(params$ms$polarity, "neg")
})

test_that("change_params_small updates all file parameters", {
  skip_if_not_installed("yaml")
  skip_on_ci()

  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  dir.create(file.path(tmpdir, "params"), recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  # Create params
  minimal_params <- list(
    ms = list(polarity = "pos"),
    files = list(
      pattern = "old",
      features = list(raw = "old_features.csv"),
      metadata = list(raw = "old_metadata.tsv"),
      annotations = list(raw = list(sirius = "old_sirius.zip")),
      spectral = list(raw = "old_spectra.mgf")
    )
  )

  yaml::write_yaml(
    minimal_params,
    file.path(tmpdir, "params/prepare_params.yaml")
  )
  yaml::write_yaml(
    minimal_params,
    file.path(tmpdir, "params/prepare_params_advanced.yaml")
  )

  # Create source directory for test files
  source_dir <- file.path(tmpdir, "test_source")
  dir.create(source_dir, recursive = TRUE)

  # Create new files in source directory
  new_files <- list(
    features = file.path(source_dir, "new_features.csv"),
    metadata = file.path(source_dir, "new_metadata.tsv"),
    sirius = file.path(source_dir, "new_sirius.zip"),
    spectra = file.path(source_dir, "new_spectra.mgf")
  )

  for (f in new_files) {
    writeLines("dummy", f)
  }

  # Update all parameters with cache_dir
  result <- change_params_small(
    fil_pat = "new_pattern",
    fil_fea_raw = new_files$features,
    fil_met_raw = new_files$metadata,
    fil_sir_raw = new_files$sirius,
    fil_spe_raw = new_files$spectra,
    ms_pol = "neg",
    cache_dir = tmpdir
  )

  # Verify updates
  params <- yaml::read_yaml(file.path(tmpdir, "params/prepare_params.yaml"))
  expect_equal(params$files$pattern, "new_pattern")
  expect_equal(params$ms$polarity, "neg")
  # Files are copied to data/source
  expect_true(grepl("new_features.csv", params$files$features$raw))
})

test_that("change_params_small handles partial updates", {
  skip_if_not_installed("yaml")
  skip_on_ci()

  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  dir.create(file.path(tmpdir, "params"), recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  # Create params
  minimal_params <- list(
    ms = list(polarity = "pos"),
    files = list(
      pattern = "original",
      features = list(raw = "features.csv"),
      metadata = list(raw = "metadata.tsv"),
      annotations = list(raw = list(sirius = "sirius.zip")),
      spectral = list(raw = "spectra.mgf")
    )
  )

  yaml::write_yaml(
    minimal_params,
    file.path(tmpdir, "params/prepare_params.yaml")
  )
  yaml::write_yaml(
    minimal_params,
    file.path(tmpdir, "params/prepare_params_advanced.yaml")
  )

  # Create source directory
  source_dir <- file.path(tmpdir, "test_files")
  dir.create(source_dir, recursive = TRUE)

  # Create files
  writeLines("dummy", file.path(source_dir, "features.csv"))
  writeLines("dummy", file.path(source_dir, "new_features.csv"))

  # Update only features file with cache_dir
  result <- change_params_small(
    fil_fea_raw = file.path(source_dir, "new_features.csv"),
    cache_dir = tmpdir
  )

  # Verify only features changed
  params <- yaml::read_yaml(file.path(tmpdir, "params/prepare_params.yaml"))
  expect_true(grepl("new_features.csv", params$files$features$raw))
  expect_equal(params$files$pattern, "original") # Unchanged
  expect_equal(params$ms$polarity, "pos") # Unchanged
})

test_that("change_params_small updates both yaml files", {
  skip_if_not_installed("yaml")
  skip_on_ci()

  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  dir.create(file.path(tmpdir, "params"), recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  withr::local_dir(tmpdir)

  # Create params
  minimal_params <- list(
    ms = list(polarity = "pos"),
    files = list(
      pattern = "test",
      features = list(raw = "features.csv"),
      metadata = list(raw = "metadata.tsv"),
      annotations = list(raw = list(sirius = "sirius.zip")),
      spectral = list(raw = "spectra.mgf")
    )
  )

  yaml::write_yaml(
    minimal_params,
    file.path(tmpdir, "params/prepare_params.yaml")
  )
  yaml::write_yaml(
    minimal_params,
    file.path(tmpdir, "params/prepare_params_advanced.yaml")
  )

  # Create files
  writeLines("dummy", "features.csv")
  writeLines("dummy", "metadata.tsv")
  writeLines("dummy", "sirius.zip")
  writeLines("dummy", "spectra.mgf")

  # Update pattern
  result <- change_params_small(fil_pat = "updated_pattern", cache_dir = ".")

  # Verify both files updated
  params1 <- yaml::read_yaml(file.path(tmpdir, "params/prepare_params.yaml"))
  params2 <- yaml::read_yaml(file.path(
    tmpdir,
    "params/prepare_params_advanced.yaml"
  ))

  expect_equal(params1$files$pattern, "updated_pattern")
  expect_equal(params2$files$pattern, "test")
})
