# Test Suite: create_edges_spectra ----

library(testthat)

test_that("create_edges_spectra validates thresholds", {
  # Create dummy mgf file with minimal content
  mgf <- c("BEGIN IONS", "PEPMASS=100", "CHARGE=1+", "100 10", "END IONS")
  writeLines(mgf, "spec.mgf")
  params <- get_params(step = "create_edges_spectra")
  expect_error(
    create_edges_spectra(input = "spec.mgf", threshold = -0.1),
    "between 0 and 1"
  )
  expect_error(
    create_edges_spectra(input = "spec.mgf", matched_peaks = 0),
    "positive integer"
  )
})

# test_that(
#   skip("Not implemented")
# )
# test_that("create_edges_spectra handles single spectrum early exit", {
#
#
#   mgf <- c("BEGIN IONS", "PEPMASS=100", "CHARGE=1+", "100 10", "END IONS")
#   writeLines(mgf, "spec.mgf")
#   out <- create_edges_spectra(
#     input = "spec.mgf",
#     threshold = 0.1,
#     matched_peaks = 1,
#     ppm = 10,
#     dalton = 0.01
#   )
#   expect_true(file.exists(out))
#   df <- tidytable::fread(out)
#   expect_true(nrow(df) == 1L || all(is.na(df[1, ])))
# })
