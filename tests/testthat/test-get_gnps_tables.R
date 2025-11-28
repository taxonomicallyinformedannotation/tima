# Test Suite: get_gnps_tables ----

library(testthat)

test_that("get_gnps_tables works with valid GNPS job ID", {
  withr::local_dir(new = temp_test_dir("get_gnps_tables_valid"))
  local_test_project(copy = TRUE)
  paths <- get_default_paths()
  params <- get_params(step = "prepare_params_advanced")

  get_file(
    url = paths$urls$examples$features,
    export = paths$data$source$features
  )
  get_file(
    url = paths$urls$examples$metadata,
    export = paths$data$source$metadata
  )

  expect_no_error(
    get_gnps_tables(
      filename = "example",
      path_features = paths$data$source$features,
      path_metadata = paths$data$source$metadata,
      path_spectra = paths$data$source$spectra,
      gnps_job_id = params$gnps$id
    )
  )
})

test_that("get_gnps_tables handles missing metadata", {
  withr::local_dir(new = temp_test_dir("get_gnps_tables_nometa"))
  local_test_project(copy = TRUE)
  paths <- get_default_paths()

  get_file(
    url = paths$urls$examples$features,
    export = paths$data$source$features
  )

  expect_no_error(
    get_gnps_tables(
      filename = "other",
      path_features = paths$data$source$features,
      path_metadata = paths$data$source$metadata,
      path_spectra = paths$data$source$spectra,
      gnps_job_id = paths$gnps$example2
    )
  )
})

test_that("get_gnps_tables works with example GNPS ID", {
  withr::local_dir(new = temp_test_dir("get_gnps_tables_example"))
  local_test_project(copy = TRUE)
  paths <- get_default_paths()

  get_file(
    url = paths$urls$examples$features,
    export = paths$data$source$features
  )
  get_file(
    url = paths$urls$examples$metadata,
    export = paths$data$source$metadata
  )

  expect_no_error(
    get_gnps_tables(
      path_features = paths$data$source$features,
      path_metadata = paths$data$source$metadata,
      path_spectra = paths$data$source$spectra,
      gnps_job_id = get_default_paths()$gnps$example
    )
  )
})

test_that("get_gnps_tables handles NULL GNPS job ID", {
  withr::local_dir(new = temp_test_dir("get_gnps_tables_null"))
  local_test_project(copy = TRUE)
  paths <- get_default_paths()

  get_file(
    url = paths$urls$examples$features,
    export = paths$data$source$features
  )

  expect_no_error(
    get_gnps_tables(
      filename = "noGNPS",
      path_features = paths$data$source$features,
      path_metadata = list(),
      path_spectra = paths$data$source$spectra,
      gnps_job_id = NULL
    )
  )
})

test_that("get_gnps_tables handles empty GNPS job ID", {
  withr::local_dir(new = temp_test_dir("get_gnps_tables_empty"))
  local_test_project(copy = TRUE)
  paths <- get_default_paths()

  get_file(
    url = paths$urls$examples$features,
    export = paths$data$source$features
  )

  expect_no_error(
    get_gnps_tables(
      filename = "noNoGNPS",
      path_features = paths$data$source$features,
      path_metadata = NULL,
      path_spectra = paths$data$source$spectra,
      gnps_job_id = ""
    )
  )
})
