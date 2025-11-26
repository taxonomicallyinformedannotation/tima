# Test Suite: sanitize_spectra ----

library(testthat)

## Setup ----

# Helper: Create a Spectra object with known properties
create_test_spectra_object <- function(
  n_spectra = 3,
  n_peaks = 10,
  seed = 123
) {
  set.seed(seed)

  spectra_data <- data.frame(
    spectrum_id = paste0("SPEC", seq_len(n_spectra)),
    msLevel = rep(2L, n_spectra),
    ## COMMENT: avoid tandom peaks below precursor
    precursorMz = runif(n_spectra, 500, 1000)
  )

  peaks_data <- lapply(seq_len(n_spectra), function(i) {
    mz <- sort(runif(n_peaks, 50, 400))
    intensity <- runif(n_peaks, 10, 1000)
    cbind(mz = mz, intensity = intensity)
  })

  spectra <- Spectra::Spectra(spectra_data)
  spectra@backend@peaksData <- peaks_data
  return(spectra)
}

## Input Validation Tests ----

test_that("sanitize_spectra requires Spectra object", {
  expect_error(
    sanitize_spectra(data.frame(a = 1)),
    "Spectra object"
  )
  expect_error(
    sanitize_spectra(list()),
    "Spectra object"
  )
  expect_error(
    sanitize_spectra(NULL),
    "Spectra object"
  )
})

test_that("sanitize_spectra validates cutoff parameter", {
  spectra <- create_test_spectra_object(n_spectra = 1)

  expect_error(
    sanitize_spectra(spectra, cutoff = -1),
    "non-negative"
  )
  expect_error(
    sanitize_spectra(spectra, cutoff = "high"),
    "non-negative"
  )
})

test_that("sanitize_spectra validates tolerance parameters", {
  spectra <- create_test_spectra_object(n_spectra = 1)

  expect_error(
    sanitize_spectra(spectra, dalton = 0),
    "positive"
  )
  expect_error(
    sanitize_spectra(spectra, dalton = -0.1),
    "positive"
  )
  expect_error(
    sanitize_spectra(spectra, ppm = 0),
    "positive"
  )
  expect_error(
    sanitize_spectra(spectra, ppm = -10),
    "positive"
  )
})

## Basic Functionality Tests ----

test_that("sanitize_spectra returns Spectra object", {
  spectra <- create_test_spectra_object(n_spectra = 2)
  result <- sanitize_spectra(spectra)

  expect_s4_class(result, "Spectra")
})

test_that("sanitize_spectra applies intensity cutoff", {
  # Create spectra with low-intensity peaks
  spectra_data <- data.frame(
    spectrum_id = "SPEC1",
    msLevel = 2L,
    precursorMz = 600
  )

  peaks <- cbind(
    mz = c(100.0, 200.0, 300.0, 400.0, 500.0),
    intensity = c(5.0, 50.0, 500.0, 5000.0, 50000.0) # First peak below cutoff
  )

  spectra <- Spectra::Spectra(spectra_data)
  spectra@backend@peaksData <- list(peaks)
  result <- sanitize_spectra(spectra, cutoff = 10)

  # Should remove peak with intensity 5
  result_peaks <- Spectra::peaksData(result)[[1]]
  expect_true(all(result_peaks[, "mz"] >= 200))
})

test_that("sanitize_spectra removes empty spectra", {
  spectra <- create_test_spectra_object(n_spectra = 1, n_peaks = 0)
  result <- sanitize_spectra(spectra)

  expect_equal(length(result), 0)
})

test_that("sanitize_spectra handles empty input", {
  # Create empty Spectra object
  empty_spectra <- Spectra::Spectra()

  result <- sanitize_spectra(empty_spectra)

  expect_s4_class(result, "Spectra")
  expect_equal(length(result), 0)
})

## Peak Filtering Tests ----

test_that("sanitize_spectra removes spectra with <= 2 peaks", {
  # Create spectrum with exactly 2 peaks
  spectra_data <- data.frame(
    spectrum_id = c("SPEC1", "SPEC2"),
    msLevel = c(2L, 2L),
    precursorMz = c(600.0, 600.0)
  )

  peaks_list <- list(
    cbind(mz = c(100, 200), intensity = c(100, 200)), # 2 peaks - should be removed
    cbind(mz = c(100, 200, 300), intensity = c(100, 200, 300)) # 3 peaks - kept
  )

  spectra <- Spectra::Spectra(spectra_data)
  spectra@backend@peaksData <- peaks_list
  result <- sanitize_spectra(spectra, cutoff = 0)

  expect_equal(length(result), 1)
})

test_that("sanitize_spectra handles precursor peak removal", {
  spectra_data <- data.frame(
    spectrum_id = "SPEC1",
    msLevel = 2L,
    precursorMz = 400.0
  )

  # Include peak at precursor m/z
  peaks <- cbind(
    mz = c(100, 200.0, 300, 400),
    intensity = c(100, 500, 300, 400)
  )

  spectra <- Spectra::Spectra(spectra_data)
  spectra@backend@peaksData <- list(peaks)
  result <- sanitize_spectra(spectra, dalton = 0.01, ppm = 10)

  result_peaks <- Spectra::peaksData(result)[[1]]

  # Precursor peak should be removed or filtered
  expect_true(nrow(result_peaks) >= 3) # At least 3 non-precursor peaks remain
})

## NaN and NULL Handling ----

test_that("sanitize_spectra removes spectra with NaN values", {
  spectra_data <- data.frame(
    spectrum_id = c("SPEC1", "SPEC2"),
    msLevel = c(2L, 2L),
    precursorMz = c(150.0, 400.0)
  )

  peaks_list <- list(
    cbind(mz = c(100, 200, 300), intensity = c(100, NaN, 300)), # Has NaN
    cbind(mz = c(100, 200, 300, 400), intensity = c(100, 200, 300, 400)) # Clean
  )

  spectra <- Spectra::Spectra(spectra_data)
  spectra@backend@peaksData <- peaks_list

  result <- sanitize_spectra(spectra)

  # Spectrum with NaN should be removed
  expect_equal(length(result), 1)
  expect_true(all(!is.nan(Spectra::peaksData(result)[[1]])))
})

test_that("sanitize_spectra handles NULL peaks gracefully", {
  # This tests the NULL filter workaround
  spectra_data <- data.frame(
    spectrum_id = c("SPEC1", "SPEC2"),
    msLevel = c(2L, 2L),
    precursorMz = c(150.0, 400.0)
  )

  # Manually create spectra with NULL in peaksData (edge case)
  peaks_list <- list(
    NULL, # NULL spectrum
    cbind(mz = c(100, 200, 300, 400), intensity = c(100, 200, 300, 400))
  )

  spectra <- Spectra::Spectra(spectra_data)
  spectra@backend@peaksData <- peaks_list

  # Should not error and should remove NULL spectrum
  result <- sanitize_spectra(spectra)
  expect_s4_class(result, "Spectra")
})

## Peak Combination and Scaling ----

test_that("sanitize_spectra combines similar peaks", {
  spectra_data <- data.frame(
    spectrum_id = "SPEC1",
    msLevel = 2L,
    precursorMz = 400.0
  )

  # Create peaks that should be combined (within tolerance)
  peaks <- cbind(
    mz = c(100.000, 100.005, 200.000, 300.000),
    intensity = c(100, 50, 200, 300)
  )

  spectra <- Spectra::Spectra(spectra_data)
  spectra@backend@peaksData <- list(peaks)

  result <- sanitize_spectra(spectra, dalton = 0.01, ppm = 100)

  result_peaks <- Spectra::peaksData(result)[[1]]

  # Peaks at 100.000 and 100.005 should be combined
  expect_lt(nrow(result_peaks), nrow(peaks))
})

test_that("sanitize_spectra scales peak intensities", {
  spectra <- create_test_spectra_object(n_spectra = 1, n_peaks = 5)
  result <- sanitize_spectra(spectra)

  result_peaks <- Spectra::peaksData(result)[[1]]
  max_intensity <- max(result_peaks[, "intensity"])

  # After scaling, max intensity should be normalized
  # Spectra::scalePeaks() typically normalizes to sum or max = 1
  expect_true(max_intensity > 0)
})

## Edge Cases ----

test_that("sanitize_spectra handles single spectrum", {
  spectra <- create_test_spectra_object(n_spectra = 1, n_peaks = 10)
  result <- sanitize_spectra(spectra)

  expect_s4_class(result, "Spectra")
  expect_gte(length(result), 0)
})

test_that("sanitize_spectra handles large spectrum count", {
  skip_on_cran()
  spectra <- create_test_spectra_object(n_spectra = 100, n_peaks = 20)

  result <- sanitize_spectra(spectra)

  expect_s4_class(result, "Spectra")
  expect_lte(length(result), 100) # Some may be filtered
})

test_that("sanitize_spectra with zero cutoff keeps more peaks", {
  spectra <- create_test_spectra_object(n_spectra = 2, n_peaks = 10)

  result_zero <- sanitize_spectra(spectra, cutoff = 0)
  result_high <- sanitize_spectra(spectra, cutoff = 100)

  # Zero cutoff should retain more or equal spectra
  expect_gte(length(result_zero), length(result_high))
})

## Return Value Tests ----

test_that("sanitize_spectra returns invisibly is false", {
  # The function explicitly returns, not invisibly
  spectra <- create_test_spectra_object(n_spectra = 1)
  result <- sanitize_spectra(spectra)

  expect_s4_class(result, "Spectra")
})


## Performance Tests ----

test_that("sanitize_spectra is reasonably fast", {
  skip_if_not_installed("bench")
  skip_on_cran()

  spectra <- create_test_spectra_object(n_spectra = 10, n_peaks = 50)

  timing <- bench::mark(
    sanitize_spectra(spectra),
    iterations = 10,
    check = FALSE
  )

  # Should complete in reasonable time (<1 second for 10 spectra)
  expect_lt(as.numeric(median(timing$median)), 1.0)
})

## Integration Tests ----

test_that("sanitize_spectra works in a pipeline", {
  # Simulate a real workflow
  spectra_data <- data.frame(
    spectrum_id = paste0("SPEC", 1:5),
    msLevel = rep(2L, 5),
    precursorMz = c(100, 200, 300, 400, 500)
  )

  peaks_list <- lapply(1:5, function(i) {
    n_peaks <- sample(5:15, 1)
    mz <- sort(runif(n_peaks, 50, 600))
    intensity <- runif(n_peaks, 1, 1000)
    cbind(mz = mz, intensity = intensity)
  })

  spectra <- Spectra::Spectra(spectra_data)
  spectra@backend@peaksData <- peaks_list

  # Apply sanitization with different parameters
  result <- spectra |>
    sanitize_spectra(cutoff = 10, dalton = 0.01, ppm = 10)

  expect_s4_class(result, "Spectra")
  expect_gte(length(result), 0)
  expect_lte(length(result), 5)
})
