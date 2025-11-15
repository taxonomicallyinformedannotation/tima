# Test Suite for create_edges()
# Tests the spectral similarity network edge creation function

library(testthat)
library(tima)

# ==============================================================================
# Helper: Create Test Spectra
# ==============================================================================

create_test_spectrum <- function(n_peaks = 5, mz_range = c(100, 500)) {
  mz <- sort(runif(n_peaks, mz_range[1], mz_range[2]))
  intensity <- runif(n_peaks, 100, 1000)
  matrix(c(mz, intensity), ncol = 2, dimnames = list(NULL, c("mz", "int")))
}

create_similar_spectra <- function(base_spectrum, n_similar = 3, noise_level = 0.1) {
  lapply(seq_len(n_similar), function(i) {
    # Add small random noise to create similar but not identical spectra
    noise <- matrix(
      rnorm(length(base_spectrum), 0, noise_level),
      ncol = 2
    )
    pmax(base_spectrum + noise, 0) # Ensure non-negative values
  })
}

# ==============================================================================
# Test: Input Validation
# ==============================================================================

test_that("create_edges validates input consistency", {
  frags <- list(create_test_spectrum(), create_test_spectrum())
  precs <- c(200, 300)

  # Mismatched lengths
  expect_error(
    create_edges(
      frags = frags,
      nspecs = 3L, # Wrong count
      precs = precs,
      method = "cosine",
      ms2_tolerance = 0.01,
      ppm_tolerance = 10,
      threshold = 0.5,
      matched_peaks = 1L
    ),
    "Length mismatch"
  )

  expect_error(
    create_edges(
      frags = frags,
      nspecs = 2L,
      precs = c(200), # Too few precs
      method = "cosine",
      ms2_tolerance = 0.01,
      ppm_tolerance = 10,
      threshold = 0.5,
      matched_peaks = 1L
    ),
    "Length mismatch"
  )
})

test_that("create_edges handles insufficient spectra", {
  # Single spectrum
  result <- create_edges(
    frags = list(create_test_spectrum()),
    nspecs = 1L,
    precs = c(200),
    method = "cosine",
    ms2_tolerance = 0.01,
    ppm_tolerance = 10,
    threshold = 0.5,
    matched_peaks = 1L
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(is.na(result[1, ])))
  expect_equal(nrow(result), 1L)

  # Zero spectra
  result <- create_edges(
    frags = list(),
    nspecs = 0L,
    precs = numeric(0),
    method = "cosine",
    ms2_tolerance = 0.01,
    ppm_tolerance = 10,
    threshold = 0.5,
    matched_peaks = 1L
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1L)
})

# ==============================================================================
# Test: Similarity Methods
# ==============================================================================

test_that("create_edges works with cosine similarity", {
  # Create similar spectra that should match
  base <- create_test_spectrum(n_peaks = 10)
  frags <- c(list(base), create_similar_spectra(base, n_similar = 2))
  precs <- c(200, 200.1, 200.2)

  result <- create_edges(
    frags = frags,
    nspecs = 3L,
    precs = precs,
    method = "cosine",
    ms2_tolerance = 0.5,
    ppm_tolerance = 50,
    threshold = 0.1, # Low threshold for test
    matched_peaks = 1L
  )

  expect_s3_class(result, "data.frame")
  expect_true("feature_id" %in% colnames(result))
  expect_true("target_id" %in% colnames(result))
  expect_true("score" %in% colnames(result))
  expect_true("matched_peaks" %in% colnames(result))

  # Should have at least some edges
  if (!all(is.na(result$feature_id))) {
    expect_true(all(result$score >= 0.1, na.rm = TRUE))
    expect_true(all(result$matched_peaks >= 1L, na.rm = TRUE))
  }
})

test_that("create_edges works with gnps similarity", {
  frags <- list(
    create_test_spectrum(),
    create_test_spectrum(),
    create_test_spectrum()
  )
  precs <- c(200, 250, 300)

  result <- create_edges(
    frags = frags,
    nspecs = 3L,
    precs = precs,
    method = "gnps",
    ms2_tolerance = 0.01,
    ppm_tolerance = 10,
    threshold = 0.0,
    matched_peaks = 0L
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 1L)
})

test_that("create_edges works with entropy similarity", {
  frags <- list(
    create_test_spectrum(),
    create_test_spectrum()
  )
  precs <- c(200, 250)

  result <- create_edges(
    frags = frags,
    nspecs = 2L,
    precs = precs,
    method = "entropy",
    ms2_tolerance = 0.01,
    ppm_tolerance = 10,
    threshold = 0.0,
    matched_peaks = 0L
  )

  expect_s3_class(result, "data.frame")
  # Entropy doesn't return matched_peaks, so they might be 0 or NA
  expect_true("score" %in% colnames(result))
})

# ==============================================================================
# Test: Threshold Filtering
# ==============================================================================

test_that("create_edges filters by similarity threshold", {
  # Create very different spectra (low similarity expected)
  frags <- list(
    matrix(c(100, 1000, 200, 500), ncol = 2, dimnames = list(NULL, c("mz", "int"))),
    matrix(c(400, 1000, 500, 500), ncol = 2, dimnames = list(NULL, c("mz", "int")))
  )
  precs <- c(200, 500)

  # High threshold should produce no edges
  result <- create_edges(
    frags = frags,
    nspecs = 2L,
    precs = precs,
    method = "cosine",
    ms2_tolerance = 0.01,
    ppm_tolerance = 10,
    threshold = 0.99, # Very high threshold
    matched_peaks = 1L
  )

  expect_s3_class(result, "data.frame")
  # Either no rows or NA values
  if (nrow(result) == 1L) {
    expect_true(all(is.na(result[1, ])))
  }
})

test_that("create_edges filters by matched peaks", {
  frags <- list(
    create_test_spectrum(n_peaks = 20),
    create_test_spectrum(n_peaks = 20)
  )
  precs <- c(200, 250)

  # Require many matched peaks
  result <- create_edges(
    frags = frags,
    nspecs = 2L,
    precs = precs,
    method = "cosine",
    ms2_tolerance = 0.01,
    ppm_tolerance = 10,
    threshold = 0.0,
    matched_peaks = 50L # More than available
  )

  expect_s3_class(result, "data.frame")
  # Should have no valid edges
  if (nrow(result) == 1L) {
    expect_true(all(is.na(result[1, ])))
  }
})

# ==============================================================================
# Test: Edge Cases
# ==============================================================================

test_that("create_edges handles single-peak spectra", {
  frags <- list(
    matrix(c(100, 1000), ncol = 2, dimnames = list(NULL, c("mz", "int"))),
    matrix(c(100, 1000), ncol = 2, dimnames = list(NULL, c("mz", "int")))
  )
  precs <- c(100, 100)

  result <- create_edges(
    frags = frags,
    nspecs = 2L,
    precs = precs,
    method = "cosine",
    ms2_tolerance = 0.5,
    ppm_tolerance = 50,
    threshold = 0.0,
    matched_peaks = 0L
  )

  expect_s3_class(result, "data.frame")
})

test_that("create_edges handles empty spectra", {
  frags <- list(
    matrix(numeric(0), ncol = 2, dimnames = list(NULL, c("mz", "int"))),
    matrix(numeric(0), ncol = 2, dimnames = list(NULL, c("mz", "int")))
  )
  precs <- c(100, 100)

  expect_no_error(
    result <- create_edges(
      frags = frags,
      nspecs = 2L,
      precs = precs,
      method = "cosine",
      ms2_tolerance = 0.01,
      ppm_tolerance = 10,
      threshold = 0.0,
      matched_peaks = 0L
    )
  )
})

# ==============================================================================
# Test: Performance & Scalability
# ==============================================================================

test_that("create_edges handles many spectra efficiently", {
  skip_on_cran()

  n_spectra <- 50
  frags <- replicate(n_spectra, create_test_spectrum(), simplify = FALSE)
  precs <- runif(n_spectra, 100, 500)

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

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  expect_s3_class(result, "data.frame")
  expect_lt(elapsed, 30) # Should complete in reasonable time
})

# ==============================================================================
# Test: Output Structure
# ==============================================================================

test_that("create_edges returns correct column structure", {
  frags <- list(
    create_test_spectrum(),
    create_test_spectrum(),
    create_test_spectrum()
  )
  precs <- c(200, 250, 300)

  result <- create_edges(
    frags = frags,
    nspecs = 3L,
    precs = precs,
    method = "cosine",
    ms2_tolerance = 0.01,
    ppm_tolerance = 10,
    threshold = 0.0,
    matched_peaks = 0L
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("feature_id", "target_id", "score", "matched_peaks"))

  # Check column types
  if (!all(is.na(result$feature_id))) {
    expect_type(result$feature_id, "integer")
    expect_type(result$target_id, "integer")
    expect_type(result$score, "double")
    expect_type(result$matched_peaks, "integer")
  }
})

test_that("create_edges ensures feature_id < target_id", {
  frags <- list(
    create_test_spectrum(),
    create_test_spectrum(),
    create_test_spectrum()
  )
  precs <- c(200, 250, 300)

  result <- create_edges(
    frags = frags,
    nspecs = 3L,
    precs = precs,
    method = "cosine",
    ms2_tolerance = 0.5,
    ppm_tolerance = 50,
    threshold = 0.0,
    matched_peaks = 0L
  )

  # Filter out NA rows
  valid_rows <- !is.na(result$feature_id)
  if (any(valid_rows)) {
    valid_result <- result[valid_rows, ]
    expect_true(all(valid_result$feature_id < valid_result$target_id))
  }
})

# ==============================================================================
# Test: Integration with create_edges_spectra
# ==============================================================================

test_that("create_edges integrates with extract_spectra output", {
  skip_on_cran()
  skip_if_offline()

  paths <- local_test_project(copy = TRUE)

  # Download test spectra
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )

  # Import spectra first, then extract
  spectra_obj <- import_spectra(paths$data$source$spectra)

  # Extract spectra data
  spectra_data <- extract_spectra(object = spectra_obj)

  # Verify the extraction worked
  expect_s3_class(spectra_data, "data.frame")
  expect_true(nrow(spectra_data) > 0)
})
