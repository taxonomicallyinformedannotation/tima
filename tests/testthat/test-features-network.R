# Test: Feature and Network Functions
library(testthat)

# test_that("prepare_features_tables handles missing RT column", {
#   copy_backbone(cache_dir = ".")
#
#   tidytable::tidytable(
#     "row ID" = 1,
#     "row m/z" = 123.4567,
#     "sample.mzML Peak area" = 98765.43
#   ) |>
#     tidytable::fwrite("data/source/example_features_no_rt.csv")
#
#   expect_no_error(
#     prepare_features_tables(
#       features = "data/source/example_features_no_rt.csv",
#       output = "data/interim/features/example_features_no_rt.tsv.gz"
#     )
#   )
#
#   unlink("data", recursive = TRUE)
# })

test_that("prepare_features_tables works with default parameters", {
  skip_on_cran()
  copy_backbone(cache_dir = ".")
  paths <- get_default_paths()

  get_file(
    url = paths$urls$examples$features,
    export = paths$data$source$features
  )

  expect_no_error(prepare_features_tables())

  unlink("data", recursive = TRUE)
})

# test_that("prepare_features_edges works", {
#   skip_on_cran()
#   copy_backbone(cache_dir = ".")
#   paths <- get_default_paths()
#
#   # Need to create edges first
#   get_file(
#     url = paths$urls$examples$spectra_mini,
#     export = paths$data$source$spectra
#   )
#   create_edges_spectra(
#     ppm = 1.0,
#     dalton = 0.001,
#     method = "entropy"
#   )
#
#   expect_no_error(prepare_features_edges())
#
#   unlink("data", recursive = TRUE)
# })

# test_that("create_components works", {
#   skip_on_cran()
#   copy_backbone(cache_dir = ".")
#   paths <- get_default_paths()
#
#   # Need edges first
#   get_file(
#     url = paths$urls$examples$spectra_mini,
#     export = paths$data$source$spectra
#   )
#   create_edges_spectra(
#     ppm = 1.0,
#     dalton = 0.001,
#     method = "entropy"
#   )
#   prepare_features_edges()
#
#   expect_no_error(create_components())
#
#   unlink("data", recursive = TRUE)
# })

# test_that("prepare_features_components works", {
#   skip_on_cran()
#   copy_backbone(cache_dir = ".")
#   paths <- get_default_paths()
#
#   # Need components first
#   get_file(
#     url = paths$urls$examples$spectra_mini,
#     export = paths$data$source$spectra
#   )
#   create_edges_spectra(
#     ppm = 1.0,
#     dalton = 0.001,
#     method = "entropy"
#   )
#   prepare_features_edges()
#   create_components()
#
#   expect_no_error(prepare_features_components())
#
#   unlink("data", recursive = TRUE)
# })
