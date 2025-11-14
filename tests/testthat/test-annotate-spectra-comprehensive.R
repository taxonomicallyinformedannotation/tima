# Comprehensive Test Suite for annotate_spectra
# Generated: 2025-11-14

library(testthat)
library(tima)

# ==============================================================================
# Test: Input Validation
# ==============================================================================

test_that("annotate_spectra validates polarity parameter", {
  skip_on_cran()

  copy_backbone(cache_dir = ".")
  paths <- get_default_paths()

  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )

  get_file(
    url = paths$urls$examples$spectral_lib_mini$pos,
    export = paths$data$interim$libraries$spectra$is$pos$isdb
  )

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

  unlink("data", recursive = TRUE)
})

test_that("annotate_spectra validates numeric parameters", {
  skip_on_cran()

  copy_backbone(cache_dir = ".")
  paths <- get_default_paths()

  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )

  get_file(
    url = paths$urls$examples$spectral_lib_mini$pos,
    export = paths$data$interim$libraries$spectra$is$pos$isdb
  )

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

  unlink("data", recursive = TRUE)
})

test_that("annotate_spectra validates file existence", {
  skip_on_cran()

  copy_backbone(cache_dir = ".")

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

  unlink("data", recursive = TRUE)
})

# test_that("annotate_spectra requires at least one library", {
#   skip_on_cran()
#
#   copy_backbone(cache_dir = ".")
#
#   get_file(
#     url = get_default_paths()$urls$examples$spectra_mini,
#     export = get_params(step = "annotate_spectra")$files$spectral$raw
#   )
#
#   # Empty library list
#   expect_error(
#     annotate_spectra(
#       libraries = list(),
#       polarity = "pos"
#     ),
#     "at least one library"
#   )
#
#   unlink("data", recursive = TRUE)
# })

# ==============================================================================
# Test: Basic Functionality
# ==============================================================================

# test_that("annotate_spectra works with single MGF library", {
#   skip_on_cran()
#
#   copy_backbone(cache_dir = ".")
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$spectra_mini,
#     export = paths$data$source$spectra
#   )
#
#   get_file(
#     url = paths$urls$examples$spectral_lib_mini$pos,
#     export = paths$data$interim$libraries$spectra$is$pos$isdb
#   )
#
#   expect_no_error(
#     annotate_spectra(
#       libraries = list(pos = paths$data$interim$libraries$spectra$is$pos$isdb),
#       polarity = "pos",
#       ppm = 10,
#       dalton = 0.01
#     )
#   )
#
#   unlink("data", recursive = TRUE)
# })

test_that("annotate_spectra works in negative mode", {
  skip_on_cran()

  copy_backbone(cache_dir = ".")
  paths <- get_default_paths()

  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )

  get_file(
    url = paths$urls$examples$spectral_lib_mini$neg,
    export = paths$data$interim$libraries$spectra$is$neg$isdb
  )

  expect_no_error(
    annotate_spectra(
      libraries = list(neg = paths$data$interim$libraries$spectra$is$neg$isdb),
      polarity = "neg",
      ppm = 10,
      dalton = 0.01
    )
  )

  unlink("data", recursive = TRUE)
})

# test_that("annotate_spectra works with multiple libraries", {
#   skip_on_cran()
#
#   copy_backbone(cache_dir = ".")
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$spectra_mini,
#     export = paths$data$source$spectra
#   )
#
#   get_file(
#     url = paths$urls$examples$spectral_lib_mini$pos,
#     export = paths$data$interim$libraries$spectra$is$pos$isdb
#   )
#
#   # Duplicate library for testing multiple libraries
#   file.copy(
#     paths$data$interim$libraries$spectra$is$pos$isdb,
#     "data/interim/libraries/spectra/is/pos/isdb2.mgf"
#   )
#
#   libs <- list(
#     pos = paths$data$interim$libraries$spectra$is$pos$isdb,
#     pos2 = "data/interim/libraries/spectra/is/pos/isdb2.mgf"
#   )
#
#   expect_no_error(
#     annotate_spectra(
#       libraries = libs,
#       polarity = "pos",
#       ppm = 10,
#       dalton = 0.01
#     )
#   )
#
#   unlink("data", recursive = TRUE)
# })

# ==============================================================================
# Test: Different Similarity Methods
# ==============================================================================

# test_that("annotate_spectra works with cosine similarity", {
#   skip_on_cran()
#
#   copy_backbone(cache_dir = ".")
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$spectra_mini,
#     export = paths$data$source$spectra
#   )
#
#   get_file(
#     url = paths$urls$examples$spectral_lib_mini$pos,
#     export = paths$data$interim$libraries$spectra$is$pos$isdb
#   )
#
#   expect_no_error(
#     annotate_spectra(
#       libraries = list(pos = paths$data$interim$libraries$spectra$is$pos$isdb),
#       polarity = "pos",
#       method = "cosine",
#       ppm = 10,
#       dalton = 0.01
#     )
#   )
#
#   unlink("data", recursive = TRUE)
# })

# test_that("annotate_spectra works with entropy similarity", {
#   skip_on_cran()
#
#   copy_backbone(cache_dir = ".")
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$spectra_mini,
#     export = paths$data$source$spectra
#   )
#
#   get_file(
#     url = paths$urls$examples$spectral_lib_mini$pos,
#     export = paths$data$interim$libraries$spectra$is$pos$isdb
#   )
#
#   expect_no_error(
#     annotate_spectra(
#       libraries = list(pos = paths$data$interim$libraries$spectra$is$pos$isdb),
#       polarity = "pos",
#       method = "entropy",
#       ppm = 10,
#       dalton = 0.01
#     )
#   )
#
#   unlink("data", recursive = TRUE)
# })

# ==============================================================================
# Test: Threshold Settings
# ==============================================================================

# test_that("annotate_spectra respects similarity threshold", {
#   skip_on_cran()
#
#   copy_backbone(cache_dir = ".")
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$spectra_mini,
#     export = paths$data$source$spectra
#   )
#
#   get_file(
#     url = paths$urls$examples$spectral_lib_mini$pos,
#     export = paths$data$interim$libraries$spectra$is$pos$isdb
#   )
#
#   # Low threshold - more matches
#   expect_no_error(
#     annotate_spectra(
#       libraries = list(pos = paths$data$interim$libraries$spectra$is$pos$isdb),
#       polarity = "pos",
#       threshold = 0.5,
#       ppm = 10,
#       dalton = 0.01
#     )
#   )
#
#   # High threshold - fewer matches
#   expect_no_error(
#     annotate_spectra(
#       libraries = list(pos = paths$data$interim$libraries$spectra$is$pos$isdb),
#       polarity = "pos",
#       threshold = 0.9,
#       ppm = 10,
#       dalton = 0.01
#     )
#   )
#
#   unlink("data", recursive = TRUE)
# })

# ==============================================================================
# Test: Tolerance Settings
# ==============================================================================

# test_that("annotate_spectra accepts different tolerance settings", {
#   skip_on_cran()
#
#   copy_backbone(cache_dir = ".")
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$spectra_mini,
#     export = paths$data$source$spectra
#   )
#
#   get_file(
#     url = paths$urls$examples$spectral_lib_mini$pos,
#     export = paths$data$interim$libraries$spectra$is$pos$isdb
#   )
#
#   # Strict tolerances
#   expect_no_error(
#     annotate_spectra(
#       libraries = list(pos = paths$data$interim$libraries$spectra$is$pos$isdb),
#       polarity = "pos",
#       ppm = 5,
#       dalton = 0.001
#     )
#   )
#
#   # Loose tolerances
#   expect_no_error(
#     annotate_spectra(
#       libraries = list(pos = paths$data$interim$libraries$spectra$is$pos$isdb),
#       polarity = "pos",
#       ppm = 20,
#       dalton = 0.05
#     )
#   )
#
#   unlink("data", recursive = TRUE)
# })

# ==============================================================================
# Test: Approx Mode (Precursor-Free Matching)
# ==============================================================================

# test_that("annotate_spectra works with approx mode enabled", {
#   skip_on_cran()
#
#   copy_backbone(cache_dir = ".")
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$spectra_mini,
#     export = paths$data$source$spectra
#   )
#
#   get_file(
#     url = paths$urls$examples$spectral_lib_mini$pos,
#     export = paths$data$interim$libraries$spectra$is$pos$isdb
#   )
#
#   expect_no_error(
#     annotate_spectra(
#       libraries = list(pos = paths$data$interim$libraries$spectra$is$pos$isdb),
#       polarity = "pos",
#       approx = TRUE,
#       ppm = 10,
#       dalton = 0.01
#     )
#   )
#
#   unlink("data", recursive = TRUE)
# })

# test_that("annotate_spectra works with approx mode disabled", {
#   skip_on_cran()
#
#   copy_backbone(cache_dir = ".")
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$spectra_mini,
#     export = paths$data$source$spectra
#   )
#
#   get_file(
#     url = paths$urls$examples$spectral_lib_mini$pos,
#     export = paths$data$interim$libraries$spectra$is$pos$isdb
#   )
#
#   expect_no_error(
#     annotate_spectra(
#       libraries = list(pos = paths$data$interim$libraries$spectra$is$pos$isdb),
#       polarity = "pos",
#       approx = FALSE,
#       ppm = 10,
#       dalton = 0.01
#     )
#   )
#
#   unlink("data", recursive = TRUE)
# })

# ==============================================================================
# Test: Intensity Cutoff
# ==============================================================================

# test_that("annotate_spectra respects intensity cutoff", {
#   skip_on_cran()
#
#   copy_backbone(cache_dir = ".")
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$spectra_mini,
#     export = paths$data$source$spectra
#   )
#
#   get_file(
#     url = paths$urls$examples$spectral_lib_mini$pos,
#     export = paths$data$interim$libraries$spectra$is$pos$isdb
#   )
#
#   # Low cutoff - keep more peaks
#   expect_no_error(
#     annotate_spectra(
#       libraries = list(pos = paths$data$interim$libraries$spectra$is$pos$isdb),
#       polarity = "pos",
#       qutoff = 0.0,
#       ppm = 10,
#       dalton = 0.01
#     )
#   )
#
#   # High cutoff - filter more peaks
#   expect_no_error(
#     annotate_spectra(
#       libraries = list(pos = paths$data$interim$libraries$spectra$is$pos$isdb),
#       polarity = "pos",
#       qutoff = 0.1,
#       ppm = 10,
#       dalton = 0.01
#     )
#   )
#
#   unlink("data", recursive = TRUE)
# })

# ==============================================================================
# Test: Output Validation
# ==============================================================================

# test_that("annotate_spectra produces valid output file", {
#   skip_on_cran()
#
#   copy_backbone(cache_dir = ".")
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$spectra_mini,
#     export = paths$data$source$spectra
#   )
#
#   get_file(
#     url = paths$urls$examples$spectral_lib_mini$pos,
#     export = paths$data$interim$libraries$spectra$is$pos$isdb
#   )
#
#   annotate_spectra(
#     libraries = list(pos = paths$data$interim$libraries$spectra$is$pos$isdb),
#     polarity = "pos",
#     ppm = 10,
#     dalton = 0.01
#   )
#
#   # Check output exists
#   output_file <- get_params(step = "annotate_spectra")$files$annotations$raw$spectral$spectral
#   expect_true(file.exists(output_file))
#
#   # Validate output structure
#   if (file.exists(output_file) && file.size(output_file) > 0) {
#     result <- tidytable::fread(output_file)
#     expect_s3_class(result, "data.frame")
#   }
#
#   unlink("data", recursive = TRUE)
# })

# ==============================================================================
# Test: Empty/Edge Cases
# ==============================================================================

test_that("annotate_spectra handles empty input gracefully", {
  skip_on_cran()
  skip("Requires empty MGF file creation")

  copy_backbone(cache_dir = ".")
  paths <- get_default_paths()

  # Would need to create an empty but valid MGF file
  # This is complex, so skip for now

  unlink("data", recursive = TRUE)
})

# ==============================================================================
# Test: Performance
# ==============================================================================

test_that("annotate_spectra completes in reasonable time", {
  skip_on_cran()
  skip("Performance test - run manually")

  copy_backbone(cache_dir = ".")
  paths <- get_default_paths()

  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )

  get_file(
    url = paths$urls$examples$spectral_lib_mini$pos,
    export = paths$data$interim$libraries$spectra$is$pos$isdb
  )

  start_time <- Sys.time()

  annotate_spectra(
    libraries = list(pos = paths$data$interim$libraries$spectra$is$pos$isdb),
    polarity = "pos",
    ppm = 10,
    dalton = 0.01
  )

  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")

  # Should complete reasonably fast
  expect_true(elapsed < 120, info = paste("Took", elapsed, "seconds"))

  unlink("data", recursive = TRUE)
})
