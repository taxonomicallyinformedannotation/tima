## Test the app
testthat::test_that("Test app", {
  ## Needed for testing
  setwd("../../..")
  get_file(
    url = "https://github.com/taxonomicallyinformedannotation/tima-example-files/raw/main/example_spectra_mini.mgf",
    export = "data/source/example_spectra.mgf"
  )
  get_file(
    url = "https://github.com/taxonomicallyinformedannotation/tima-example-files/raw/main/example_features.csv",
    export = "data/source/example_features.csv"
  )
  setwd("app/")

  ## Actual testing
  app <- ShinyDriver$new()
  app$snapshotInit("test_app")
  app$uploadFile(fil_spe_raw = "../data/source/example_spectra.mgf")
  app$uploadFile(fil_fea_raw = "../data/source/example_features.csv")
  app$setInputs(save = "click")
  app$snapshot()
  testthat::succeed()
})
