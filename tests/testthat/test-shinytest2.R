## Testing the Shiny app
library(shinytest2)

testthat::test_that({
  app <- AppDriver$new(name = "tima-r", seed = 42)
  app$upload_file(fil_spe_raw = "data/source/example_spectra.mgf")
  app$upload_file(fil_fea_raw = "data/source/example_features.csv")
  app$click("save")
  app$click("launch")
})
