library(shinytest2)

testthat::test_that(desc = "{shinytest2} recording: app", code = {
  tima:::get_file(
    url = "https://github.com/taxonomicallyinformedannotation/tima-example-files/raw/main/example_spectra_mini.mgf",
    export = "data/source/example_spectra.mgf"
  )
  tima:::get_file(
    url = "https://github.com/taxonomicallyinformedannotation/tima-example-files/raw/main/example_features.csv",
    export = "data/source/example_features.csv"
  )

  app <- AppDriver$new(
    app_dir = system.file("app.R", package = "tima"),
    timeout = 100000,
    load_timeout = 100000
  )

  app$upload_file(fil_spe_raw = "data/source/example_spectra.mgf")
  app$upload_file(fil_fea_raw = "data/source/example_features.csv")
  app$click("save")
  app$expect_values()
})
