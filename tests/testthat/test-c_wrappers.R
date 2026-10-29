# Test Suite: c_wrappers ----

library(testthat)

# Use clean, sorted spectra matrices to satisfy C-wrapper preconditions.
sp_a <- cbind(mz = c(100.000, 150.000, 200.000), intensity = c(100, 50, 25))
sp_b <- cbind(mz = c(100.005, 149.995, 300.000), intensity = c(90, 45, 10))
pmz <- 500.0

test_that("gnps_wrapper returns numeric score, match count, forward and reverse", {
  res <- gnps_wrapper(sp_a, sp_b)
  expect_type(res, "double")
  expect_length(res, 4L)
  expect_true(is.finite(res[1L]))
  expect_true(is.finite(res[2L]))
  expect_gte(res[1L], 0)
  expect_lte(res[1L], 1)
  expect_gte(res[2L], 0)
  expect_gte(res[3L], 0)
  expect_lte(res[3L], 1)
  expect_gte(res[4L], 0)
  expect_lte(res[4L], 1)
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

test_that("gnps_chain_dp_wrapper returns score, matches, forward and reverse when requested", {
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
  expect_length(res, 4L)
  expect_gte(res[1L], 0)
  expect_lte(res[1L], 1)
  expect_gte(res[2L], 0)
  # forward >= score (uses matched library only, so unmatched lib doesn't dilute)
  expect_gte(res[3L], res[1L] - 1e-12)
  expect_lte(res[3L], 1)
  # reverse >= score (uses matched query only, so unmatched query doesn't dilute)
  expect_gte(res[4L], res[1L] - 1e-12)
  expect_lte(res[4L], 1)
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

test_that("join_gnps_wrapper returns no pairs for non-overlapping masses", {
  res <- join_gnps_wrapper(
    x = c(100, 200),
    y = c(1000, 2000),
    xPrecursorMz = 500,
    yPrecursorMz = 500,
    tolerance = 0.001,
    ppm = 1
  )

  # Outer-join semantics: unmatched peaks are retained with NA counterpart.
  expect_equal(length(res[[1L]]), 4L)
  expect_equal(length(res[[2L]]), 4L)
  expect_equal(sum(is.na(res[[1L]])), 2L)
  expect_equal(sum(is.na(res[[2L]])), 2L)
})

test_that("gnps_chain_dp_wrapper can match shifted peaks when precursor differs", {
  x <- cbind(mz = c(100, 200), intensity = c(100, 80))
  y <- cbind(mz = c(110, 210), intensity = c(95, 70))

  res <- gnps_chain_dp_wrapper(
    x = x,
    y = y,
    xPrecursorMz = 500,
    yPrecursorMz = 510,
    tolerance = 0.01,
    ppm = 5,
    matchedPeaksCount = TRUE
  )

  expect_gte(res[1L], 0)
  expect_gte(res[2L], 1)
})

test_that("gnps_wrapper handles pre-joined matrices with NA peaks", {
  x_joined <- cbind(mz = c(100, NA, 200), intensity = c(50, NA, 30))
  y_joined <- cbind(mz = c(100, 150, NA), intensity = c(45, 20, NA))

  res <- gnps_wrapper(x_joined, y_joined)
  expect_length(res, 4L)
  expect_true(is.finite(res[1L]))
})

test_that("gnps_chain_dp_wrapper validates matrix-like inputs", {
  expect_error(
    gnps_chain_dp_wrapper(
      x = c(1, 2, 3),
      y = sp_b,
      xPrecursorMz = pmz,
      yPrecursorMz = pmz,
      tolerance = 0.01,
      ppm = 10,
      matchedPeaksCount = TRUE
    )
  )
})

test_that("gnps_wrapper validates matrix-like inputs", {
  expect_error(gnps_wrapper(c(1, 2, 3), sp_b))
})

# ---- forward / reverse dot product tests ------------------------------------

test_that("identical spectra yield forward = reverse = 1", {
  res <- gnps_chain_dp_wrapper(
    x = sp_a,
    y = sp_a,
    xPrecursorMz = pmz,
    yPrecursorMz = pmz,
    tolerance = 0.01,
    ppm = 10,
    matchedPeaksCount = TRUE
  )
  expect_equal(res[3L], 1.0, tolerance = 1e-6)
  expect_equal(res[4L], 1.0, tolerance = 1e-6)
})

test_that("reverse > forward when query has more unmatched peaks than library", {
  # query has 4 peaks, library has 2 matching peaks + 1 non-matching
  # query has 2 unmatched, library has 1 unmatched
  # reverse ignores unmatched query (2 peaks) -> bigger boost
  # forward ignores unmatched library (1 peak) -> smaller boost
  q <- cbind(mz = c(100, 200, 300, 400), intensity = c(100, 50, 25, 10))
  l <- cbind(mz = c(100.005, 200.005, 900), intensity = c(90, 45, 30))
  res <- gnps_chain_dp_wrapper(
    x = q,
    y = l,
    xPrecursorMz = pmz,
    yPrecursorMz = pmz,
    tolerance = 0.01,
    ppm = 10,
    matchedPeaksCount = TRUE
  )
  # Both should be >= score
  expect_gte(res[3L], res[1L] - 1e-12)
  expect_gte(res[4L], res[1L] - 1e-12)
  # reverse > forward because query has more unmatched peaks
  expect_gt(res[4L], res[3L])
})

test_that("forward and reverse are 0 when no peaks match", {
  q <- cbind(mz = c(100, 200), intensity = c(50, 50))
  l <- cbind(mz = c(500, 600), intensity = c(50, 50))
  res <- gnps_chain_dp_wrapper(
    x = q,
    y = l,
    xPrecursorMz = pmz,
    yPrecursorMz = pmz,
    tolerance = 0.01,
    ppm = 10,
    matchedPeaksCount = TRUE
  )
  expect_equal(res[3L], 0.0)
  expect_equal(res[4L], 0.0)
})

# Cross-path properties on random spectra ----

.random_spectrum_pair <- function(n_shared = 5L, n_extra = 4L) {
  x <- cbind(sort(stats::runif(8L, 50, 400)), stats::runif(8L, 1, 100))
  shared <- x[sample(nrow(x), n_shared), , drop = FALSE]
  shared[, 1L] <- shared[, 1L] + stats::rnorm(n_shared, 0, 0.003)
  y <- rbind(
    shared,
    cbind(stats::runif(n_extra, 50, 400), stats::runif(n_extra, 1, 100))
  )
  list(x = x, y = y[order(y[, 1L]), , drop = FALSE])
}

test_that("join then aligned scoring agrees with the chain DP scorer", {
  set.seed(3L)
  for (i in seq_len(20L)) {
    sp <- .random_spectrum_pair()
    j <- join_gnps_wrapper(
      sp$x[, 1L],
      sp$y[, 1L],
      NA_real_,
      NA_real_,
      tolerance = 0.01,
      ppm = 0
    )
    aligned <- gnps_wrapper(
      sp$x[j$x, , drop = FALSE],
      sp$y[j$y, , drop = FALSE]
    )
    chain <- gnps_chain_dp_wrapper(
      sp$x,
      sp$y,
      NA_real_,
      NA_real_,
      tolerance = 0.01,
      ppm = 0,
      matchedPeaksCount = TRUE
    )
    expect_equal(aligned[[1L]], chain[[1L]], tolerance = 1e-12)
    expect_identical(aligned[[2L]], chain[[2L]])
  }
})

test_that("chain DP score is symmetric and swaps forward with reverse", {
  set.seed(7L)
  for (i in seq_len(20L)) {
    sp <- .random_spectrum_pair()
    xy <- gnps_chain_dp_wrapper(
      sp$x,
      sp$y,
      NA_real_,
      NA_real_,
      0.01,
      0,
      matchedPeaksCount = TRUE
    )
    yx <- gnps_chain_dp_wrapper(
      sp$y,
      sp$x,
      NA_real_,
      NA_real_,
      0.01,
      0,
      matchedPeaksCount = TRUE
    )
    expect_equal(xy[[1L]], yx[[1L]], tolerance = 1e-12)
    expect_identical(xy[[2L]], yx[[2L]])
    expect_equal(xy[[3L]], yx[[4L]], tolerance = 1e-12)
    expect_equal(xy[[4L]], yx[[3L]], tolerance = 1e-12)
  }
})
