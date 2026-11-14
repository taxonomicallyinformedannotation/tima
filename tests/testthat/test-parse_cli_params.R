# Test Suite: parse_cli_params ----

library(testthat)

test_that("parse_cli_params merges CLI arguments into parameters", {
  local_test_project(copy = TRUE)
  params <- get_params(step = "prepare_params_advanced")

  arguments <- list()
  arguments$ann_can_fin <- 666L

  result <- parse_cli_params(arguments = arguments, parameters = params)

  expect_type(result, "list")
  expect_equal(
    result$annotations$candidates$final,
    666L,
    info = "CLI argument should override parameter"
  )
})

test_that("parse_cli_params handles empty arguments list", {
  local_test_project(copy = TRUE)
  params <- get_params(step = "prepare_params_advanced")

  result <- parse_cli_params(arguments = list(), parameters = params)

  expect_type(result, "list")
  expect_identical(
    result,
    params,
    info = "Empty args should return original params"
  )
})

test_that("parse_cli_params preserves unmodified parameters", {
  local_test_project(copy = TRUE)
  params <- get_params(step = "prepare_params_advanced")

  # Modify only one parameter
  arguments <- list(ann_can_fin = 100L)

  result <- parse_cli_params(arguments = arguments, parameters = params)

  expect_type(result, "list")
  # Other parameters should remain unchanged
  expect_true(length(result) >= length(params))
})

test_that("parse_cli_params maps SIRIUS analog mz-error CLI argument", {
  local_test_project(copy = TRUE)
  params <- get_params(step = "prepare_params_advanced")

  result <- parse_cli_params(
    arguments = list(
      too_sir_max_analog_abs_mz_error = 0.005,
      ms_clu = "H2O, CH3OH",
      ms_sol = "ACN, MeOH"
    ),
    parameters = params
  )

  expect_equal(
    result$tools$sirius$max_analog_abs_mz_error,
    0.005
  )
  expect_equal(result$ms$clusters, c("H2O", "CH3OH"))
  expect_equal(result$ms$solvents, c("ACN", "MeOH"))
})

test_that("docopt options round-trip through parse_cli_params", {
  doc_wt <- paste(
    readLines(
      system.file(
        "scripts",
        "docopt",
        "weight_annotations.txt",
        package = "tima"
      )
    ),
    collapse = "\n"
  )
  args_wt <- docopt::docopt(doc_wt, args = "--wei-che-11=0.9")
  res_wt <- parse_cli_params(
    arguments = args_wt,
    parameters = list(
      weights = list(chemical = list(cla = list(kingdom = 0.5)))
    )
  )
  expect_identical(res_wt$weights$chemical$cla$kingdom, 0.9)

  doc_rt <- paste(
    readLines(
      system.file(
        "scripts",
        "docopt",
        "prepare_libraries_rt.txt",
        package = "tima"
      )
    ),
    collapse = "\n"
  )
  args_rt <- docopt::docopt(doc_rt, args = "--names-mgf-rt=RT")
  res_rt <- parse_cli_params(
    arguments = args_rt,
    parameters = list(names = list(mgf = list(retention_time = "RTINSECONDS")))
  )
  expect_identical(res_rt$names$mgf$retention_time, "RT")

  doc_sp <- paste(
    readLines(
      system.file(
        "scripts",
        "docopt",
        "prepare_annotations_spectra.txt",
        package = "tima"
      )
    ),
    collapse = "\n"
  )
  args_sp <- docopt::docopt(doc_sp, args = "--fil-ann-raw-spe=foo.tsv")
  res_sp <- parse_cli_params(
    arguments = args_sp,
    parameters = list(
      files = list(
        annotations = list(raw = list(spectral = list(spectral = "")))
      )
    )
  )
  expect_identical(
    res_sp$files$annotations$raw$spectral$spectral,
    "foo.tsv"
  )
})
