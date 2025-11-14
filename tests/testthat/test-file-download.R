# Test: File Download and GNPS Functions
library(testthat)

test_that("replace_id handles different input combinations", {
  # Empty GNPS ID
  result1 <- replace_id(
    x = "example/123456_features.tsv",
    user_gnps = "",
    user_filename = "Foo"
  )
  expect_type(result1, "character")

  # Non-empty GNPS ID
  result2 <- replace_id(
    x = "example/123456_features.tsv",
    user_gnps = "Foo",
    user_filename = "Foo"
  )
  expect_type(result2, "character")

  # Default example GNPS ID
  result3 <- replace_id(
    x = "example/123456_features.tsv",
    user_gnps = get_default_paths()$gnps$example,
    user_filename = "Foo"
  )
  expect_type(result3, "character")
})

test_that("get_file downloads and handles existing files", {
  copy_backbone(cache_dir = ".")
  paths <- get_default_paths()

  # First download
  expect_no_error(
    get_file(
      url = paths$urls$examples$features,
      export = paths$data$source$features
    )
  )
  expect_true(file.exists(paths$data$source$features))

  # Second download (should skip)
  expect_message(
    get_file(
      url = paths$urls$examples$features,
      export = paths$data$source$features
    ),
    regexp = "already exists"
  )

  unlink("data", recursive = TRUE)
})

test_that("get_file fails gracefully with invalid URL", {
  expect_error(
    get_file(
      url = "InVaLiDUrL",
      export = "InValidFile.txt"
    )
  )
})

test_that("get_gnps_tables works with valid GNPS job ID", {
  copy_backbone(cache_dir = ".")
  paths <- get_default_paths()
  params <- get_params(step = "prepare_params_advanced")

  # Download required files first
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

  unlink("data", recursive = TRUE)
})

test_that("get_gnps_tables handles missing metadata", {
  copy_backbone(cache_dir = ".")
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

  unlink("data", recursive = TRUE)
})

test_that("get_gnps_tables works with example GNPS ID", {
  copy_backbone(cache_dir = ".")
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

  unlink("data", recursive = TRUE)
})

test_that("get_gnps_tables handles NULL GNPS job ID", {
  copy_backbone(cache_dir = ".")
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

  unlink("data", recursive = TRUE)
})

test_that("get_gnps_tables handles empty GNPS job ID", {
  copy_backbone(cache_dir = ".")
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

  unlink("data", recursive = TRUE)
})

