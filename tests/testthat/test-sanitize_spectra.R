# Test Suite: sanitize_spectra ----

library(testthat)

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
  expect_s4_class(res, "Spectra")
  expect_equal(length(res), 0L)
})

test_that("sanitize_spectra processes simple spectra", {
  sp <- make_spectra()
  res <- sanitize_spectra(sp, cutoff = 0, dalton = 0.01, ppm = 10)
  expect_s4_class(res, "Spectra")
  expect_true(length(res) <= length(sp))
})

test_that("sanitize_spectra handles FEATURE_ID column", {
  df <- data.frame(
    FEATURE_ID = c("FT001", "FT002", "FT003"),
    mz = c(list(123.4567, 234.5678, 345.6789))
  ) |>
    Spectra::Spectra() |>
    sanitize_spectra()

  expect_s4_class(df, "Spectra")
  expect_true("FEATURE_ID" %in% colnames(df@backend@spectraData))
})

test_that("sanitize_spectra handles SLAW_ID column", {
  df <- data.frame(
    SLAW_ID = c("FT001", "FT002", "FT003"),
    mz = c(list(123.4567, 234.5678, 345.6789))
  ) |>
    Spectra::Spectra() |>
    sanitize_spectra()

  expect_s4_class(df, "Spectra")
  expect_true("SLAW_ID" %in% colnames(df@backend@spectraData))
})
