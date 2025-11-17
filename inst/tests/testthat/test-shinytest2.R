library(shinytest2)

test_that("{shinytest2} recording: inst", {
  tima:::get_file(
    url = "https://github.com/taxonomicallyinformedannotation/tima-example-files/raw/main/example_spectra_mini.mgf",
    export = "data/source/example_spectra.mgf"
  )
  tima:::get_file(
    url = "https://github.com/taxonomicallyinformedannotation/tima-example-files/raw/main/example_features.csv",
    export = "data/source/example_features.csv"
  )
  tima:::get_file(
    url = "https://github.com/taxonomicallyinformedannotation/tima-example-files/raw/main/example_metadata.tsv",
    export = "data/source/example_metadata.tsv"
  )
  app <- AppDriver$new(
    name = "inst",
    height = 1038,
    width = 1499,
    timeout = 100000,
    load_timeout = 100000,
    shiny_args = list(
      host = "127.0.0.1",
      port = 3838,
      launch.browser = FALSE
    )
  )
  app$upload_file(fil_spe_raw = "data/source/example_spectra.mgf")
  app$upload_file(fil_fea_raw = "data/source/example_features.csv")
  app$upload_file(fil_met_raw = "data/source/example_metadata.tsv")
  app$click("save")
  ## COMMENT See <https://github.com/rstudio/shinytest2/issues/417>
  # app$expect_screenshot()
  # app$expect_values()
  app$stop()
})
