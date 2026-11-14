library(shinytest2)

test_that("{shinytest2} recording: inst", {
  # Downloads the example files from GitHub
  skip_on_cran()
  skip_if(
    identical(tolower(Sys.getenv("TIMA_TESTS_OFFLINE")), "true"),
    "TIMA_TESTS_OFFLINE=true"
  )
  skip_if_offline("github.com")
  td <- withr::local_tempdir()
  spectra_path <- file.path(td, "example_spectra.mgf")
  features_path <- file.path(td, "example_features.csv")
  metadata_path <- file.path(td, "example_metadata.tsv")
  tima::get_file(
    url = "https://github.com/taxonomicallyinformedannotation/tima-example-files/raw/main/example_spectra_mini.mgf",
    export = spectra_path
  )
  tima::get_file(
    url = "https://github.com/taxonomicallyinformedannotation/tima-example-files/raw/main/example_features.csv",
    export = features_path
  )
  tima::get_file(
    url = "https://github.com/taxonomicallyinformedannotation/tima-example-files/raw/main/example_metadata.tsv",
    export = metadata_path
  )
  app <- AppDriver$new(
    name = "inst",
    height = 1038,
    width = 1499,
    timeout = 100000,
    load_timeout = 100000,
    shiny_args = list(
      host = "127.0.0.1",
      launch.browser = FALSE
    ),
    variant = NULL
  )
  withr::defer(app$stop())
  app$upload_file(fil_spe_raw = spectra_path)
  app$upload_file(fil_fea_raw = features_path)
  app$upload_file(fil_met_raw = metadata_path)
  app$click("save")
  ## COMMENT See <https://github.com/rstudio/shinytest2/issues/417>
  # app$expect_screenshot()
  skip_on_os("windows")
  app$expect_values()
})
