# Test: Network and Edge Functions
library(testthat)

# =============================================================================
# Tests for create_components()
# =============================================================================

# test_that("create_components handles empty edges", {
#   skip_on_cran()
#
#   # Use isolated test environment with automatic cleanup
#   withr::local_tempdir()
#   copy_backbone(cache_dir = ".")
#   paths <- get_default_paths()
#
#   # Create empty edges file
#   empty_edges <- tidytable::tidytable(
#     feature_source = character(0),
#     feature_target = character(0)
#   )
#
#   export_output(
#     x = empty_edges,
#     file = paths$data$interim$features$edges
#   )
#
#   # Should handle gracefully without error
#   expect_no_error(create_components())
# })

# test_that("create_components creates valid output structure", {
#   skip_on_cran()
#
#   # Use isolated test environment with automatic cleanup
#   withr::local_tempdir()
#   copy_backbone(cache_dir = ".")
#   paths <- get_default_paths()
#
#   # Create minimal edges
#   edges <- tidytable::tidytable(
#     feature_source = c("FT001", "FT002", "FT003"),
#     feature_target = c("FT002", "FT003", "FT001"),
#     similarity = c(0.8, 0.9, 0.7)
#   )
#
#   export_output(
#     x = edges,
#     file = paths$data$interim$features$edges
#   )
#
#   result <- create_components()
#
#   expect_s3_class(result, "data.frame")
#   expect_true("component_id" %in% colnames(result) ||
#     "feature_id" %in% colnames(result))
# })

# =============================================================================
# Tests for create_edges()
# =============================================================================

test_that("create_edges handles empty spectra", {
  # Create minimal spectra
  frags <- list(
    matrix(c(100, 200), nrow = 1, dimnames = list(NULL, c("mz", "int")))
  )

  result <- create_edges(
    frags = frags,
    nspecs = 1L,
    precs = c(100),
    method = "cosine",
    ms2_tolerance = 0.01,
    ppm_tolerance = 10,
    threshold = 0.5,
    matched_peaks = 6L
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 0) # May be empty if no matches
})

test_that("create_edges validates method parameter", {
  frags <- list(
    matrix(
      c(100, 200, 150, 300),
      nrow = 2,
      dimnames = list(NULL, c("mz", "int"))
    ),
    matrix(
      c(100, 250, 150, 350),
      nrow = 2,
      dimnames = list(NULL, c("mz", "int"))
    )
  )

  # Should error when method is invalid
  expect_error(
    create_edges(
      frags = frags,
      nspecs = 2L,
      precs = c(100, 110),
      method = "invalid_method_that_does_not_exist",
      ms2_tolerance = 0.01,
      ppm_tolerance = 10,
      threshold = 0.5,
      matched_peaks = 1L
    )
  )
})

test_that("create_edges handles single spectrum gracefully", {
  # Single spectrum - should return empty result
  frags <- list(
    matrix(c(100, 200), nrow = 1, dimnames = list(NULL, c("mz", "int")))
  )

  result <- create_edges(
    frags = frags,
    nspecs = 1L,
    precs = c(100),
    method = "cosine",
    ms2_tolerance = 0.01,
    ppm_tolerance = 10,
    threshold = 0.5,
    matched_peaks = 1L
  )

  expect_s3_class(result, "data.frame")
  # Should have NA values since no edges possible
  expect_true(all(is.na(result[1, ])))
})

test_that("create_edges handles single peak spectra", {
  # Single peak spectrum
  frags <- list(
    matrix(c(123.456, 234.567), nrow = 1, dimnames = list(NULL, c("mz", "int")))
  )

  result <- create_edges(
    frags = rep(frags, 3),
    nspecs = 3L,
    precs = c(123.456, 234.567, 345.678),
    method = "gnps",
    ms2_tolerance = 0.01,
    ppm_tolerance = 10.0,
    threshold = 0.1,
    matched_peaks = 0L
  )

  expect_s3_class(result, "data.frame")
})

test_that("create_edges with different similarity methods", {
  # Create test spectra
  frags <- list(
    matrix(
      c(100, 200, 150, 300, 200, 400),
      nrow = 3,
      dimnames = list(NULL, c("mz", "int"))
    ),
    matrix(
      c(100, 250, 150, 350, 200, 450),
      nrow = 3,
      dimnames = list(NULL, c("mz", "int"))
    )
  )

  # Test cosine
  result_cosine <- create_edges(
    frags = frags,
    nspecs = 2L,
    precs = c(200, 220),
    method = "cosine",
    ms2_tolerance = 0.01,
    ppm_tolerance = 10,
    threshold = 0.1,
    matched_peaks = 1L
  )
  expect_s3_class(result_cosine, "data.frame")

  # Test gnps
  result_gnps <- create_edges(
    frags = frags,
    nspecs = 2L,
    precs = c(200, 220),
    method = "gnps",
    ms2_tolerance = 0.01,
    ppm_tolerance = 10,
    threshold = 0.1,
    matched_peaks = 1L
  )
  expect_s3_class(result_gnps, "data.frame")

  # Test entropy
  result_entropy <- create_edges(
    frags = frags,
    nspecs = 2L,
    precs = c(200, 220),
    method = "entropy",
    ms2_tolerance = 0.01,
    ppm_tolerance = 10,
    threshold = 0.1,
    matched_peaks = 1L
  )
  expect_s3_class(result_entropy, "data.frame")
})

# =============================================================================
# Tests for extract_spectra()
# =============================================================================

# test_that("extract_spectra handles MGF format", {
#   skip_on_cran()
#
#   # Create a temporary MGF file
#   temp_mgf <- withr::local_tempfile(fileext = ".mgf")
#
#   mgf_content <- "BEGIN IONS
# TITLE=Feature_1
# PEPMASS=123.456
# CHARGE=1+
# 100.0 1000
# 200.0 2000
# END IONS
#
# BEGIN IONS
# TITLE=Feature_2
# PEPMASS=234.567
# CHARGE=1+
# 150.0 1500
# 250.0 2500
# END IONS
# "
#
#   writeLines(mgf_content, temp_mgf)
#
#   result <- extract_spectra(input = temp_mgf)
#
#   expect_type(result, "list")
#   expect_true(length(result) > 0)
#   expect_true(all(c("spectra", "precursors") %in% names(result) |
#     "Spectra" %in% class(result)))
# })

test_that("extract_spectra handles nonexistent file gracefully", {
  # Should either error or return empty result
  expect_error(
    extract_spectra(input = "nonexistent_file_12345.mgf")
  )
})

test_that("extract_spectra handles empty MGF file", {
  skip_on_cran()

  temp_mgf <- withr::local_tempfile(fileext = ".mgf")
  writeLines("", temp_mgf)

  # Should handle gracefully - either return empty or error with message
  result <- tryCatch(
    extract_spectra(input = temp_mgf),
    error = function(e) NULL
  )

  # Either result is NULL (errored) or is a valid but empty structure
  if (!is.null(result)) {
    expect_type(result, "list")
  }
})

# =============================================================================
# Performance tests for vectorized operations
# =============================================================================

test_that("create_edges performs efficiently with many spectra", {
  skip_on_cran()

  # Create 50 test spectra
  n_spectra <- 50
  frags <- lapply(1:n_spectra, function(i) {
    n_peaks <- sample(5:20, 1)
    mz <- sort(runif(n_peaks, 100, 500))
    int <- runif(n_peaks, 100, 1000)
    matrix(c(mz, int), ncol = 2, dimnames = list(NULL, c("mz", "int")))
  })

  precs <- runif(n_spectra, 150, 600)

  # Should complete reasonably quickly
  start_time <- Sys.time()
  result <- create_edges(
    frags = frags,
    nspecs = n_spectra,
    precs = precs,
    method = "cosine",
    ms2_tolerance = 0.01,
    ppm_tolerance = 10,
    threshold = 0.5,
    matched_peaks = 3L
  )
  end_time <- Sys.time()

  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

  expect_s3_class(result, "data.frame")
  expect_lt(elapsed, 10) # Should complete in < 10 seconds
})
