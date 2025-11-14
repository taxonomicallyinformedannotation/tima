# Test: Annotation Preparation Functions
library(testthat)

test_that("prepare_annotations_gnps handles missing input", {
  copy_backbone(cache_dir = ".")

  expect_no_error(
    prepare_annotations_gnps(input = "fileDoesNotExist")
  )
  expect_no_error(
    prepare_annotations_gnps(input = NULL)
  )

  unlink("data", recursive = TRUE)
})

test_that("prepare_annotations_gnps works with default parameters", {
  skip_on_cran()
  copy_backbone(cache_dir = ".")
  paths <- get_default_paths()

  # Setup GNPS data
  get_file(
    url = paths$urls$examples$features,
    export = paths$data$source$features
  )
  get_file(
    url = paths$urls$examples$metadata,
    export = paths$data$source$metadata
  )

  params <- get_params(step = "prepare_params_advanced")
  get_gnps_tables(
    filename = "example",
    path_features = paths$data$source$features,
    path_metadata = paths$data$source$metadata,
    path_spectra = paths$data$source$spectra,
    gnps_job_id = params$gnps$id
  )

  expect_no_error(prepare_annotations_gnps())

  unlink("data", recursive = TRUE)
})

test_that("prepare_annotations_sirius handles NULL input", {
  copy_backbone(cache_dir = ".")

  expect_no_error(
    prepare_annotations_sirius(input_directory = NULL)
  )

  unlink("data", recursive = TRUE)
})

test_that("prepare_annotations_sirius handles non-existent directory", {
  copy_backbone(cache_dir = ".")

  expect_no_error(
    prepare_annotations_sirius(input_directory = "randomDirThatDoesNotExist")
  )
  expect_no_error(
    prepare_annotations_sirius(input_directory = "randomDirThatDoesNotExist.xyz")
  )

  unlink("data", recursive = TRUE)
})

test_that("prepare_annotations_sirius works with version 5", {
  skip_on_cran()
  copy_backbone(cache_dir = ".")

  get_example_sirius()

  expect_no_error(
    prepare_annotations_sirius(
      input_directory = "data/interim/annotations/example_sirius_5.zip",
      sirius_version = 5
    )
  )

  unlink("data", recursive = TRUE)
})

test_that("prepare_annotations_sirius works with default parameters", {
  skip_on_cran()
  copy_backbone(cache_dir = ".")

  get_example_sirius()

  expect_no_error(prepare_annotations_sirius())

  unlink("data", recursive = TRUE)
})

test_that("prepare_annotations_spectra works", {
  skip_on_cran()
  copy_backbone(cache_dir = ".")
  paths <- get_default_paths()

  # Setup spectral library and structures
  get_file(
    url = paths$urls$examples$spectral_lib_mini$pos,
    export = paths$data$interim$libraries$spectra$is$pos$isdb
  )
  get_file(
    url = paths$urls$sop$isdb,
    export = paths$data$interim$libraries$sop$isdb
  )
  get_file(
    url = paths$urls$examples$structures_processed,
    export = paths$data$interim$libraries$sop$merged$structures$processed
  )

  prepare_libraries_spectra()

  # Need to create annotation file first
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  annotate_spectra(
    libraries = list(
      pos = "data/interim/libraries/spectra/exp/internal_pos.rds"
    ),
    ppm = 1.0,
    dalton = 0.001
  )

  expect_no_error(prepare_annotations_spectra())

  unlink("data", recursive = TRUE)
})

