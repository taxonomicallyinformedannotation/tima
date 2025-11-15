# Test: Utility and Helper Functions
library(testthat)

test_that("change_params_small updates parameters correctly", {
  paths <- local_test_project(copy = TRUE)

  get_example_files()

  expect_no_error(
    change_params_small(
      fil_pat = "myExamplePattern",
      fil_fea_raw = paths$data$source$features,
      fil_met_raw = paths$data$source$metadata,
      fil_sir_raw = "data/interim/annotations/example_sirius.zip",
      fil_spe_raw = paths$data$source$spectra,
      ms_pol = "pos",
      org_tax = "Gentiana lutea",
      hig_con = TRUE,
      summarize = FALSE
    )
  )
})

test_that("parse_cli_params merges CLI arguments correctly", {
  local_test_project(copy = TRUE)
  params <- get_params(step = "prepare_params_advanced")

  arguments <- list()
  arguments$ann_can_fin <- 666L

  result <- parse_cli_params(arguments = arguments, parameters = params)

  expect_type(result, "list")
  expect_equal(result$annotations$candidates$final, 666L)
})

test_that("get_example_files works in cache mode", {
  paths <- local_test_project(copy = TRUE)

  expect_no_error(
    get_example_files(
      example = c(
        "features",
        "metadata",
        "sirius",
        "spectra",
        "spectral_lib_with_rt"
      ),
      in_cache = FALSE
    )
  )
})

test_that("get_example_files works with default parameters", {
  skip_on_cran()

  expect_no_error(get_example_files())
})

# test_that("install function works in test mode", {
#   expect_no_error(install(test = TRUE))
# })
