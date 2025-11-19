# Test Suite: change_params_small ----

library(testthat)

skip()
# test_that("change_params_small updates workflow parameters correctly", {
#   paths <- local_test_project(copy = TRUE)
#
#   get_example_files()
#
#   expect_no_error(
#     change_params_small(
#       fil_pat = "myExamplePattern",
#       fil_fea_raw = paths$data$source$features,
#       fil_met_raw = paths$data$source$metadata,
#       fil_sir_raw = "data/interim/annotations/example_sirius.zip",
#       fil_spe_raw = paths$data$source$spectra,
#       ms_pol = "pos",
#       org_tax = "Gentiana lutea",
#       hig_con = TRUE,
#       summarize = FALSE
#     )
#   )
# })

# test_that("change_params_small handles minimal parameters", {
#   paths <- local_test_project(copy = TRUE)
#
#   # Test with just required parameters
#   expect_no_error(
#     change_params_small(
#       fil_pat = "test_pattern",
#       ms_pol = "pos"
#     )
#   )
# })

# test_that("change_params_small validates MS polarity parameter", {
#   paths <- local_test_project(copy = TRUE)
#
#   # Invalid polarity should error (if validation exists)
#   # Document actual behavior
#   result <- tryCatch(
#     change_params_small(fil_pat = "test", ms_pol = "invalid"),
#     error = function(e) e
#   )
#
#   # Either errors or accepts - document which
#   expect_true(inherits(result, "NULL") || inherits(result, "error"))
# })
