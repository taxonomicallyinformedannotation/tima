# ==============================================================================
# Test Suite: sanitize_spectra
# ==============================================================================
library(testthat)
library(tima)

make_spectra <- function() {
  df <- data.frame(
    FEATURE_ID = c("FT001", "FT002"),
    mz = I(list(c(100, 150, 200), c(110, 160, 210))),
    intensity = I(list(c(10, 200, 500), c(5, 180, 450)))
  )
  Spectra::Spectra(df)
}

test_that("sanitize_spectra validates input type", {
  expect_error(sanitize_spectra(spectra = list()), "Spectra object")
})

test_that("sanitize_spectra handles empty object", {
  empty <- Spectra::Spectra(data.frame())
  res <- sanitize_spectra(empty)
  expect_s3_class(res, "Spectra")
  expect_equal(length(res), 0L)
})

test_that("sanitize_spectra processes simple spectra", {
  sp <- make_spectra()
  res <- sanitize_spectra(sp, cutoff = 0, dalton = 0.01, ppm = 10)
  expect_s3_class(res, "Spectra")
  expect_true(length(res) <= length(sp))
})
