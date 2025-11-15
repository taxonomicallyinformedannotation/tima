# Test: Library Preparation Functions
library(testthat)

test_that("prepare_libraries_spectra handles non-existent input", {
  local_test_project(copy = TRUE)

  expect_no_error(
    prepare_libraries_spectra(
      input = "doesNotExists.txt",
      nam_lib = "nope"
    )
  )
})

test_that("prepare_libraries_spectra handles NULL input", {
  local_test_project(copy = TRUE)

  expect_no_error(
    prepare_libraries_spectra(
      input = NULL,
      nam_lib = "null"
    )
  )
})

test_that("prepare_libraries_spectra works with default parameters", {
  skip_on_cran()
  local_test_project(copy = TRUE)
  paths <- get_default_paths()

  # Download required files
  get_file(
    url = paths$urls$examples$spectral_lib_mini$pos,
    export = paths$data$interim$libraries$spectra$is$pos$isdb
  )
  get_file(
    url = paths$urls$examples$spectral_lib_mini$neg,
    export = paths$data$interim$libraries$spectra$is$neg$isdb
  )

  expect_no_error(prepare_libraries_spectra())
})

# test_that("prepare_libraries_spectra warns when library already exists", {
#   skip_on_cran()
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$spectral_lib_mini$pos,
#     export = paths$data$interim$libraries$spectra$is$pos$isdb
#   )
#   get_file(
#     url = paths$urls$examples$spectral_lib_mini$neg,
#     export = paths$data$interim$libraries$spectra$is$neg$isdb
#   )
#
#   # First run
#   prepare_libraries_spectra()
#
#   # Second run should warn
#   expect_message(
#     prepare_libraries_spectra(),
#     regexp = "already"
#   )
# })

# test_that("prepare_libraries_rt works with experimental data", {
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$spectral_lib_mini$with_rt,
#     export = paths$data$source$libraries$spectra$exp$with_rt
#   )
#   get_file(
#     url = paths$urls$examples$lib_mini$rt,
#     export = paths$data$source$libraries$rt$example_mini
#   )
#
#   expect_no_error(
#     prepare_libraries_rt(
#       mgf_exp = list(pos = paths$data$source$libraries$spectra$exp$with_rt),
#       temp_exp = paths$data$source$libraries$rt$example_mini
#     )
#   )
# })

# test_that("prepare_libraries_rt works with in-silico data", {
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$spectral_lib_mini$with_rt,
#     export = paths$data$source$libraries$spectra$exp$with_rt
#   )
#   get_file(
#     url = paths$urls$examples$lib_mini$rt,
#     export = paths$data$source$libraries$rt$example_mini
#   )
#
#   expect_no_error(
#     prepare_libraries_rt(
#       mgf_is = list(pos = paths$data$source$libraries$spectra$exp$with_rt),
#       temp_is = paths$data$source$libraries$rt$example_mini
#     )
#   )
# })

# test_that("prepare_libraries_rt warns on invalid SMILES", {
#   local_test_project(copy = TRUE)
#
#   tidytable::tidytable(
#     "rt" = 0.1,
#     "smiles" = "wrongSMILES",
#     "inchikey" = NA
#   ) |>
#     tidytable::fwrite("data/source/libraries/rt/example_bad.tsv")
#
#   expect_warning(
#     prepare_libraries_rt(
#       temp_exp = "data/source/libraries/rt/example_bad.tsv"
#     )
#   )
# })

test_that("prepare_libraries_sop_closed works with and without input", {
  local_test_project(copy = TRUE)
  paths <- get_default_paths()

  # Try with some input (will fail gracefully)
  expect_no_error(
    prepare_libraries_sop_closed(input = paths$data$source$libraries$sop$lotus)
  )

  # Try without input
  expect_no_error(
    prepare_libraries_sop_closed()
  )
})

test_that("prepare_libraries_sop_ecmdb handles missing input", {
  local_test_project(copy = TRUE)

  expect_no_error(
    prepare_libraries_sop_ecmdb(input = "randomNonExistingFile")
  )
  expect_no_error(
    prepare_libraries_sop_ecmdb()
  )
})

test_that("prepare_libraries_sop_hmdb handles missing input", {
  local_test_project(copy = TRUE)

  expect_no_error(
    prepare_libraries_sop_hmdb(input = "randomNonExistingFile")
  )
  expect_no_error(
    prepare_libraries_sop_hmdb()
  )
})

test_that("prepare_libraries_sop_lotus handles missing input", {
  local_test_project(copy = TRUE)

  expect_no_error(
    prepare_libraries_sop_lotus(input = "randomNonExistingFile")
  )
  expect_no_error(
    prepare_libraries_sop_lotus()
  )
})

# test_that("prepare_libraries_sop_merged works with filtering", {
#   skip_on_cran()
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   # Need LOTUS data
#   fake_lotus(export = paths$data$source$libraries$sop$lotus)
#   prepare_libraries_sop_lotus()
#
#   expect_no_error(
#     prepare_libraries_sop_merged(
#       files = get_params(
#         step = "prepare_libraries_sop_merged"
#       )$files$libraries$sop$prepared$lotus,
#       filter = TRUE,
#       level = "family",
#       value = "Simaroubaceae|Gentianaceae",
#       output_key = "data/interim/libraries/sop/merged/bitter.tsv.gz"
#     )
#   )
# })

test_that("prepare_libraries_sop_merged triggers SMILES processing", {
  skip_on_cran()
  local_test_project(copy = TRUE)
  paths <- get_default_paths()

  # Create fake data
  fake_ecmdb(export = paths$data$source$libraries$sop$ecmdb)
  fake_lotus(export = paths$data$source$libraries$sop$lotus)

  prepare_libraries_sop_ecmdb()
  prepare_libraries_sop_lotus()
  prepare_libraries_sop_closed()

  expect_no_error(
    prepare_libraries_sop_merged(
      files = c(
        get_params(
          step = "prepare_libraries_sop_merged"
        )$files$libraries$sop$prepared$closed,
        get_params(
          step = "prepare_libraries_sop_merged"
        )$files$libraries$sop$prepared$ecmdb,
        get_params(
          step = "prepare_libraries_sop_merged"
        )$files$libraries$sop$prepared$lotus
      )
    )
  )
})
