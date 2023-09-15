## Test the app
testthat::test_that("Test app", {
  setwd("../../")
  app <- ShinyDriver$new()
  app$snapshotInit("test_app")
  app$uploadFile(fil_spe_raw = "../../data/source/example_spectra.mgf")
  app$uploadFile(fil_fea_raw = "../../data/source/example_features.csv")
  app$setInputs(save = "click")
  app$snapshot()
  testthat::succeed()
})
