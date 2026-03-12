# Test Suite: sanitize_spectrum_matrix / is_spectrum_sanitized ----
# Covers the two low-level matrix helpers that guard the C-level GNPS scorer.

library(testthat)

# ---- is_spectrum_sanitized ---------------------------------------------------

test_that("is_spectrum_sanitized returns TRUE for a clean sorted matrix", {
  sp <- cbind(mz = c(100, 150, 200), intensity = c(0.3, 0.5, 0.2))
  expect_true(tima:::is_spectrum_sanitized(sp))
})

test_that("is_spectrum_sanitized returns TRUE for NULL (no matrix → trivially ok)", {
  expect_true(tima:::is_spectrum_sanitized(NULL))
})

test_that("is_spectrum_sanitized returns FALSE for unsorted mz", {
  sp <- cbind(mz = c(200, 100, 150), intensity = c(1, 1, 1))
  expect_false(tima:::is_spectrum_sanitized(sp))
})

test_that("is_spectrum_sanitized returns FALSE when mz contains NaN", {
  sp <- cbind(mz = c(100, NaN, 200), intensity = c(1, 1, 1))
  expect_false(tima:::is_spectrum_sanitized(sp))
})

test_that("is_spectrum_sanitized returns FALSE when intensity contains NA", {
  sp <- cbind(mz = c(100, 150, 200), intensity = c(1, NA_real_, 1))
  expect_false(tima:::is_spectrum_sanitized(sp))
})

test_that("is_spectrum_sanitized returns FALSE for duplicate mz within tolerance", {
  # Two peaks at 100.000 and 100.005 — within dalton=0.01 → duplicate
  sp <- cbind(mz = c(100.000, 100.005, 200.0), intensity = c(0.5, 0.5, 0.1))
  expect_false(tima:::is_spectrum_sanitized(sp, tolerance = 0.01, ppm = 0))
})

test_that("is_spectrum_sanitized returns TRUE for single-row matrix", {
  sp <- cbind(mz = 100, intensity = 1)
  expect_true(tima:::is_spectrum_sanitized(sp))
})

test_that("is_spectrum_sanitized returns FALSE for non-matrix", {
  expect_false(tima:::is_spectrum_sanitized(data.frame(mz = 1, intensity = 1)))
  expect_false(tima:::is_spectrum_sanitized(c(1, 2, 3)))
})

test_that("is_spectrum_sanitized returns FALSE for matrix with < 2 columns", {
  sp <- matrix(c(1, 2, 3), ncol = 1)
  expect_false(tima:::is_spectrum_sanitized(sp))
})

test_that("is_spectrum_sanitized never errors or returns NA on garbage input", {
  bad_inputs <- list(list(), TRUE, 42L, matrix(character(0), ncol = 2))
  for (x in bad_inputs) {
    result <- tima:::is_spectrum_sanitized(x)
    expect_type(result, "logical")
    expect_false(is.na(result))
  }
})

# ---- sanitize_spectrum_matrix ------------------------------------------------

test_that("sanitize_spectrum_matrix sorts unsorted mz", {
  sp <- cbind(mz = c(300, 100, 200), intensity = c(1, 2, 3))
  out <- tima:::sanitize_spectrum_matrix(sp)
  expect_true(!is.unsorted(out[, "mz"]))
  expect_equal(out[, "mz"], c(100, 200, 300))
})

test_that("sanitize_spectrum_matrix removes NaN rows", {
  sp <- cbind(mz = c(100, NaN, 200), intensity = c(1, 1, 1))
  out <- tima:::sanitize_spectrum_matrix(sp)
  expect_equal(nrow(out), 2L)
  expect_false(any(is.nan(out)))
})

test_that("sanitize_spectrum_matrix removes zero/negative intensity rows", {
  sp <- cbind(mz = c(100, 150, 200), intensity = c(1, 0, -0.5))
  out <- tima:::sanitize_spectrum_matrix(sp)
  expect_equal(nrow(out), 1L)
  expect_equal(out[1, "mz"] |> unname(), 100)
})

test_that("sanitize_spectrum_matrix merges peaks within tolerance", {
  # Two peaks at 100.00 and 100.005 are within dalton=0.01 → must merge
  sp <- cbind(mz = c(100.000, 100.005, 200.0), intensity = c(2, 8, 1))
  out <- tima:::sanitize_spectrum_matrix(sp, tolerance = 0.01, ppm = 0)
  expect_equal(nrow(out), 2L)
  # Merged m/z should be intensity-weighted: (100*2 + 100.005*8)/10 = 100.004
  expect_equal(
    out[1, "mz"] |> unname(),
    (100.000 * 2 + 100.005 * 8) / 10,
    tolerance = 1e-9
  )
  # Merged intensity is the sum
  expect_equal(out[1, "intensity"] |> unname(), 10)
})

test_that("sanitize_spectrum_matrix returns 0-row matrix when all peaks invalid", {
  sp <- cbind(mz = c(NaN, NaN), intensity = c(0, 0))
  out <- tima:::sanitize_spectrum_matrix(sp)
  expect_equal(nrow(out), 0L)
  expect_equal(ncol(out), 2L)
})

test_that("sanitize_spectrum_matrix passes through already-clean matrix unchanged", {
  sp <- cbind(mz = c(100, 150, 200), intensity = c(0.3, 0.5, 0.2))
  out <- tima:::sanitize_spectrum_matrix(sp)
  expect_equal(out, sp)
})

test_that("sanitize_spectrum_matrix returns input for empty or non-matrix", {
  empty <- matrix(numeric(0), ncol = 2)
  expect_equal(tima:::sanitize_spectrum_matrix(empty), empty)

  not_matrix <- data.frame(mz = 1)
  expect_equal(tima:::sanitize_spectrum_matrix(not_matrix), not_matrix)
})

# ---- round-trip: sanitize then is_sanitized ----------------------------------

test_that("output of sanitize_spectrum_matrix always passes is_spectrum_sanitized", {
  set.seed(42)
  # Messy: unsorted, NaN, near-duplicate mz, zero intensity
  sp_messy <- cbind(
    mz = c(300, 100, NaN, 100.005, 200, 150),
    intensity = c(0.5, 0.3, 1, 0.2, 0, 0.1)
  )
  out <- tima:::sanitize_spectrum_matrix(sp_messy, tolerance = 0.01, ppm = 0)
  if (nrow(out) >= 2L) {
    expect_true(tima:::is_spectrum_sanitized(out, tolerance = 0.01, ppm = 0))
  }
})
