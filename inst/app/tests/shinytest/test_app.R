## Test the app
testthat::test_that("Test app", {
  ## Needed for testing
  setwd("../../../../")
  system(
    command =
      "
    mkdir inst
    cp -R params inst/params
    cp paths.yaml inst/paths.yaml
    "
  )
  paths <- parse_yaml_paths()
  params <- get_params(step = "prepare_params")
  get_gnps_tables(
    filename = "example",
    path_features = paths$data$source$features,
    path_metadata = paths$data$source$metadata,
    path_spectra = paths$data$source$spectra,
    gnps_job_id = params$gnps$id
  )
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  setwd("inst/app/")

  ## Actual testing
  app <- ShinyDriver$new()
  app$snapshotInit("test_app")
  app$uploadFile(fil_spe_raw = "../../data/source/example_spectra.mgf")
  app$uploadFile(fil_fea_raw = "../../data/source/example_features.csv")
  app$setInputs(save = "click")
  app$snapshot()
  testthat::succeed()
})
