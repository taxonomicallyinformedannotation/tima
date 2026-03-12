# Test Suite: c_wrappers ----

library(testthat)

# Use clean, sorted spectra matrices to satisfy C-wrapper preconditions.
sp_a <- cbind(mz = c(100.000, 150.000, 200.000), intensity = c(100, 50, 25))
sp_b <- cbind(mz = c(100.005, 149.995, 300.000), intensity = c(90, 45, 10))
pmz <- 500.0

test_that("gnps_wrapper returns numeric score and match count", {
  res <- gnps_wrapper(sp_a, sp_b)
  expect_type(res, "double")
  expect_length(res, 2L)
  expect_true(is.finite(res[1L]))
  expect_true(is.finite(res[2L]))
  expect_gte(res[1L], 0)
  expect_lte(res[1L], 1)
  expect_gte(res[2L], 0)
})

test_that("gnps_wrapper gives maximal similarity for identical spectra", {
  res <- gnps_wrapper(sp_a, sp_a)
  expect_equal(res[1L], 1.0, tolerance = 1e-6)
  expect_gte(res[2L], 1)
})

test_that("join_gnps_wrapper returns paired index vectors", {
  res <- join_gnps_wrapper(
    x = sp_a[, 1L],
    y = sp_b[, 1L],
    xPrecursorMz = pmz,
    yPrecursorMz = pmz,
    tolerance = 0.01,
    ppm = 10
  )

  expect_type(res, "list")
  expect_length(res, 2L)
  expect_type(res[[1L]], "integer")
  expect_type(res[[2L]], "integer")
  expect_equal(length(res[[1L]]), length(res[[2L]]))
})

test_that("gnps_chain_dp_wrapper returns score only by default", {
  score <- gnps_chain_dp_wrapper(
    x = sp_a,
    y = sp_b,
    xPrecursorMz = pmz,
    yPrecursorMz = pmz,
    tolerance = 0.01,
    ppm = 10,
    matchedPeaksCount = FALSE
  )

  expect_type(score, "double")
  expect_length(score, 1L)
  expect_gte(score, 0)
  expect_lte(score, 1)
})

test_that("gnps_chain_dp_wrapper returns score and matches when requested", {
  res <- gnps_chain_dp_wrapper(
    x = sp_a,
    y = sp_b,
    xPrecursorMz = pmz,
    yPrecursorMz = pmz,
    tolerance = 0.01,
    ppm = 10,
    matchedPeaksCount = TRUE
  )

  expect_type(res, "double")
  expect_length(res, 2L)
  expect_gte(res[1L], 0)
  expect_lte(res[1L], 1)
  expect_gte(res[2L], 0)
})

test_that("gnps_chain_dp_wrapper gives maximal similarity for identical spectra", {
  score <- gnps_chain_dp_wrapper(
    x = sp_a,
    y = sp_a,
    xPrecursorMz = pmz,
    yPrecursorMz = pmz,
    tolerance = 0.01,
    ppm = 10,
    matchedPeaksCount = FALSE
  )

  expect_equal(score, 1.0, tolerance = 1e-6)
})
