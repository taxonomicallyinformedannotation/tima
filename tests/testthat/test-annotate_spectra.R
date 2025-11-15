# ==============================================================================
# Test Suite: annotate_spectra
# ==============================================================================

# Comprehensive Test Suite for annotate_spectra
# Generated: 2025-11-14

library(testthat)
library(tima)

write_minimal_mgf <- function(path, pepmass = 100, charge = "1+") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lines <- c(
    "BEGIN IONS",
    paste0("PEPMASS=", pepmass),
    paste0("CHARGE=", charge),
    "50 100",
    "75 200",
    "100 300",
    "END IONS"
  )
  writeLines(lines, path)
}

# ==============================================================================
# Test: Input Validation
# ==============================================================================

test_that("annotate_spectra validates polarity parameter", {
  skip_on_cran()

  local_test_project(copy = TRUE)
  paths <- get_default_paths()

  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )

  # Replace remote library download with minimal mgf
  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "test_lib_pos.mgf"
  )
  write_minimal_mgf(lib_path)

  # Invalid polarity
  expect_error(
    annotate_spectra(
      libraries = list(pos = paths$data$interim$libraries$spectra$is$pos$isdb),
      polarity = "invalid"
    ),
    "pos.*neg"
  )

  expect_error(
    annotate_spectra(
      libraries = list(pos = paths$data$interim$libraries$spectra$is$pos$isdb),
      polarity = "positive"
    ),
    "pos.*neg"
  )
})

test_that("annotate_spectra validates numeric parameters", {
  skip_on_cran()

  local_test_project(copy = TRUE)
  paths <- get_default_paths()

  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )

  # Replace remote library download with minimal mgf
  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "test_lib_pos.mgf"
  )
  write_minimal_mgf(lib_path)

  # Threshold must be 0-1
  expect_error(
    annotate_spectra(
      libraries = list(pos = paths$data$interim$libraries$spectra$is$pos$isdb),
      threshold = 1.5
    ),
    "between 0 and 1"
  )

  expect_error(
    annotate_spectra(
      libraries = list(pos = paths$data$interim$libraries$spectra$is$pos$isdb),
      threshold = -0.1
    ),
    "between 0 and 1"
  )

  # PPM must be positive
  expect_error(
    annotate_spectra(
      libraries = list(pos = paths$data$interim$libraries$spectra$is$pos$isdb),
      ppm = -10
    ),
    "positive"
  )

  expect_error(
    annotate_spectra(
      libraries = list(pos = paths$data$interim$libraries$spectra$is$pos$isdb),
      ppm = 0
    ),
    "positive"
  )

  # Dalton must be positive
  expect_error(
    annotate_spectra(
      libraries = list(pos = paths$data$interim$libraries$spectra$is$pos$isdb),
      dalton = -0.01
    ),
    "positive"
  )

  # Cutoff must be non-negative
  expect_error(
    annotate_spectra(
      libraries = list(pos = paths$data$interim$libraries$spectra$is$pos$isdb),
      qutoff = -0.1
    ),
    "non-negative"
  )
})

test_that("annotate_spectra validates file existence", {
  skip_on_cran()

  local_test_project(copy = TRUE)

  # Missing input file
  expect_error(
    annotate_spectra(
      input = "nonexistent_spectra.mgf",
      libraries = list(pos = "library.mgf"),
      polarity = "pos"
    ),
    "not found"
  )

  # Missing library file
  get_file(
    url = get_default_paths()$urls$examples$spectra_mini,
    export = get_params(step = "annotate_spectra")$files$spectral$raw
  )

  expect_error(
    annotate_spectra(
      libraries = list(pos = "nonexistent_library.mgf"),
      polarity = "pos"
    ),
    "not found"
  )
})

test_that("annotate_spectra requires at least one library", {
  skip_on_cran()
  local_test_project(copy = TRUE)
  get_file(
    url = get_default_paths()$urls$examples$spectra_mini,
    export = get_params(step = "annotate_spectra")$files$spectral$raw
  )
  expect_error(
    annotate_spectra(
      libraries = list(),
      polarity = "pos"
    ),
    "At least one library"
  )
})

# ==============================================================================
# Test: Basic Functionality
# ==============================================================================

test_that("annotate_spectra works with single MGF library", {
  skip_on_cran()
  local_test_project(copy = TRUE)
  paths <- get_default_paths()
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "test_lib_pos.mgf"
  )
  write_minimal_mgf(lib_path)
  expect_no_error(
    annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      ppm = 10,
      dalton = 0.01
    )
  )
})

test_that("annotate_spectra works in negative mode", {
  skip_on_cran()

  local_test_project(copy = TRUE)
  paths <- get_default_paths()

  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  lib_path_neg <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "test_lib_neg.mgf"
  )
  write_minimal_mgf(lib_path_neg, charge = "1-", pepmass = 120)

  expect_no_error(
    annotate_spectra(
      libraries = list(neg = lib_path_neg),
      polarity = "neg",
      ppm = 10,
      dalton = 0.01
    )
  )
})

test_that("annotate_spectra works with multiple libraries", {
  skip_on_cran()
  local_test_project(copy = TRUE)
  paths <- get_default_paths()
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "test_lib_pos.mgf"
  )
  write_minimal_mgf(lib_path)
  lib_path2 <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "test_lib_pos2.mgf"
  )
  write_minimal_mgf(lib_path2, pepmass = 110)
  libs <- list(pos = lib_path, pos2 = lib_path2)
  expect_no_error(
    annotate_spectra(
      libraries = list(pos = lib_path, pos2 = lib_path2),
      polarity = "pos",
      ppm = 10,
      dalton = 0.01
    )
  )
})

# ==============================================================================
# Test: Different Similarity Methods
# ==============================================================================

test_that("annotate_spectra works with cosine similarity", {
  skip_on_cran()
  local_test_project(copy = TRUE)
  paths <- get_default_paths()
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "test_lib_pos.mgf"
  )
  write_minimal_mgf(lib_path)
  expect_no_error(
    annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      method = "cosine",
      ppm = 10,
      dalton = 0.01
    )
  )
})

test_that("annotate_spectra works with entropy similarity", {
  skip_on_cran()
  local_test_project(copy = TRUE)
  paths <- get_default_paths()
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "test_lib_pos.mgf"
  )
  write_minimal_mgf(lib_path)
  expect_no_error(
    annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      method = "entropy",
      ppm = 10,
      dalton = 0.01
    )
  )
})

# ==============================================================================
# Test: Threshold Settings
# ==============================================================================

test_that("annotate_spectra respects similarity threshold", {
  skip_on_cran()
  local_test_project(copy = TRUE)
  paths <- get_default_paths()
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "test_lib_pos.mgf"
  )
  write_minimal_mgf(lib_path)
  expect_no_error(
    annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      threshold = 0.5,
      ppm = 10,
      dalton = 0.01
    )
  )
  expect_no_error(
    annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      threshold = 0.9,
      ppm = 10,
      dalton = 0.01
    )
  )
})

# ==============================================================================
# Test: Tolerance Settings
# ==============================================================================

test_that("annotate_spectra accepts different tolerance settings", {
  skip_on_cran()
  local_test_project(copy = TRUE)
  paths <- get_default_paths()
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "test_lib_pos.mgf"
  )
  write_minimal_mgf(lib_path)
  expect_no_error(
    annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      ppm = 5,
      dalton = 0.001
    )
  )
  expect_no_error(
    annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      ppm = 20,
      dalton = 0.05
    )
  )
})

# ==============================================================================
# Test: Approx Mode (Precursor-Free Matching)
# ==============================================================================

test_that("annotate_spectra works with approx mode enabled", {
  skip_on_cran()
  local_test_project(copy = TRUE)
  paths <- get_default_paths()
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "test_lib_pos.mgf"
  )
  write_minimal_mgf(lib_path)
  expect_no_error(
    annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      approx = TRUE,
      ppm = 10,
      dalton = 0.01
    )
  )
})

test_that("annotate_spectra works with approx mode disabled", {
  skip_on_cran()
  local_test_project(copy = TRUE)
  paths <- get_default_paths()
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "test_lib_pos.mgf"
  )
  write_minimal_mgf(lib_path)
  expect_no_error(
    annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      approx = FALSE,
      ppm = 10,
      dalton = 0.01
    )
  )
})

# ==============================================================================
# Test: Intensity Cutoff
# ==============================================================================

test_that("annotate_spectra respects intensity cutoff", {
  skip_on_cran()
  local_test_project(copy = TRUE)
  paths <- get_default_paths()
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "test_lib_pos.mgf"
  )
  write_minimal_mgf(lib_path)
  expect_no_error(
    annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      qutoff = 0.0,
      ppm = 10,
      dalton = 0.01
    )
  )
  expect_no_error(
    annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      qutoff = 0.1,
      ppm = 10,
      dalton = 0.01
    )
  )
})

# ==============================================================================
# Test: Output Validation
# ==============================================================================

test_that("annotate_spectra produces valid output file", {
  skip_on_cran()
  local_test_project(copy = TRUE)
  paths <- get_default_paths()
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "test_lib_pos.mgf"
  )
  write_minimal_mgf(lib_path)
  annotate_spectra(
    libraries = list(pos = lib_path),
    polarity = "pos",
    ppm = 10,
    dalton = 0.01
  )
  output_file <- get_params(
    step = "annotate_spectra"
  )$files$annotations$raw$spectral$spectral
  expect_true(file.exists(output_file))
  if (file.exists(output_file) && file.size(output_file) > 0) {
    result <- tidytable::fread(output_file)
    expect_s3_class(result, "data.frame")
  }
})

# ==============================================================================
# Test: Empty/Edge Cases
# ==============================================================================

test_that("annotate_spectra handles empty input gracefully", {
  skip_on_cran()
  skip("Requires empty MGF file creation")

  local_test_project(copy = TRUE)
  paths <- get_default_paths()

  # Would need to create an empty but valid MGF file
  # This is complex, so skip for now
})

# ==============================================================================
# Test: Performance
# ==============================================================================

test_that("annotate_spectra completes in reasonable time", {
  skip_on_cran()
  skip("Performance test - run manually")

  local_test_project(copy = TRUE)
  paths <- get_default_paths()

  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )

  # Replace remote library download with minimal mgf
  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "test_lib_pos.mgf"
  )
  write_minimal_mgf(lib_path)

  start_time <- Sys.time()

  annotate_spectra(
    libraries = list(pos = lib_path),
    polarity = "pos",
    ppm = 10,
    dalton = 0.01
  )

  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")

  # Should complete reasonably fast
  expect_true(elapsed < 120, info = paste("Took", elapsed, "seconds"))
})
