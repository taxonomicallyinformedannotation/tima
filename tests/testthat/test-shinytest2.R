library(shinytest2)
library(tima)

test_that("{shinytest2} recording: app", {
  get_file(url = "https://github.com/taxonomicallyinformedannotation/tima-example-files/raw/main/example_spectra_mini.mgf", export = "data/source/example_spectra.mgf")
  get_file(url = "https://github.com/taxonomicallyinformedannotation/tima-example-files/raw/main/example_features.csv", export = "data/source/example_features.csv")
  app <- AppDriver$new(
    timeout = 100000,
    load_timeout = 100000,
    shiny_args = list(
      appDir = system.file(package = "tima"),
      host = "127.0.0.1",
      port = 3838,
      launch.browser = TRUE
    )
  )
  app$upload_file(fil_spe_raw = "data/source/example_spectra.mgf")
  app$upload_file(fil_fea_raw = "data/source/example_features.csv")
  app$click("save")
  app$expect_values()
})
