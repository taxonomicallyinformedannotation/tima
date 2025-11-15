# ==============================================================================
# Test Fixtures and Helpers
# ==============================================================================
#
# This file contains reusable test fixtures and helper functions to reduce
# boilerplate code and improve test maintainability.

# ==============================================================================
# Environment Setup Fixtures
# ==============================================================================

#' Setup an isolated temporary project directory for filesystem tests
#'
#' @description
#' Creates a temporary test environment with the package structure.
#' Automatically cleaned up after test completion via withr.
#'
#' @param copy Logical. If TRUE, copies the package backbone into temp directory
#'
#' @return Named list of default paths from get_default_paths()
#'
#' @examples
#' test_that("some test", {
#'   paths <- local_test_project()
#'   # Test code here
#' })
local_test_project <- function(copy = TRUE) {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  # Suppress logs during tests for cleaner output
  local_quiet_logging()

  if (isTRUE(copy)) {
    copy_backbone(cache_dir = ".")
  }

  get_default_paths()
}

#' Quiet logging within a test scope
#'
#' @description
#' Suppresses verbose logging output during tests while preserving warnings
#' and errors.
#'
#' @param threshold Character or logger level. Minimum log level to display.
#'   Default: "WARN"
#'
#' @return Invisible NULL
#'
#' @examples
#' test_that("quiet test", {
#'   local_quiet_logging("ERROR")
#'   # Only errors will be logged
#' })
local_quiet_logging <- function(threshold = "WARN") {
  if (requireNamespace("logger", quietly = TRUE)) {
    # Convert character to logger level if needed
    lvl <- tryCatch(
      get(threshold, envir = asNamespace("logger")),
      error = function(...) threshold
    )
    logger::log_threshold(lvl)
  }
  invisible(NULL)
}

# ==============================================================================
# Test Data Fixtures - Spectral Data
# ==============================================================================

#' Create a minimal test spectrum
#'
#' @description
#' Generates a simple mass spectrum for testing spectral processing functions.
#'
#' @param n_peaks Integer. Number of peaks in the spectrum. Default: 5
#' @param mz_range Numeric vector of length 2. m/z range for peaks. Default: c(100, 500)
#' @param intensity_range Numeric vector of length 2. Intensity range. Default: c(100, 1000)
#'
#' @return Matrix with two columns (mz, int) and n_peaks rows
#'
#' @examples
#' spectrum <- create_test_spectrum(n_peaks = 10)
create_test_spectrum <- function(
  n_peaks = 5L,
  mz_range = c(100, 500),
  intensity_range = c(100, 1000)
) {
  stopifnot(
    is.numeric(n_peaks),
    n_peaks >= 0,
    length(mz_range) == 2,
    length(intensity_range) == 2
  )

  if (n_peaks == 0) {
    return(matrix(numeric(0), ncol = 2, dimnames = list(NULL, c("mz", "int"))))
  }

  mz <- sort(runif(n_peaks, mz_range[1], mz_range[2]))
  intensity <- runif(n_peaks, intensity_range[1], intensity_range[2])

  matrix(
    c(mz, intensity),
    ncol = 2,
    dimnames = list(NULL, c("mz", "int"))
  )
}

#' Create similar spectra for similarity testing
#'
#' @description
#' Generates multiple spectra similar to a base spectrum by adding noise.
#' Useful for testing spectral similarity functions.
#'
#' @param base_spectrum Matrix. Base spectrum to create variations from
#' @param n_similar Integer. Number of similar spectra to generate. Default: 3
#' @param noise_level Numeric. Standard deviation of noise to add. Default: 0.1
#'
#' @return List of n_similar spectrum matrices
#'
#' @examples
#' base <- create_test_spectrum()
#' similar <- create_similar_spectra(base, n_similar = 5)
create_similar_spectra <- function(
  base_spectrum,
  n_similar = 3L,
  noise_level = 0.1
) {
  stopifnot(
    is.matrix(base_spectrum),
    ncol(base_spectrum) == 2,
    n_similar > 0,
    noise_level >= 0
  )

  lapply(seq_len(n_similar), function(i) {
    noise <- matrix(
      rnorm(length(base_spectrum), 0, noise_level),
      ncol = 2
    )
    # Ensure non-negative values
    pmax(base_spectrum + noise, 0)
  })
}

# ==============================================================================
# Test Data Fixtures - Features and Annotations
# ==============================================================================

#' Create minimal features table for testing
#'
#' @description
#' Generates a simple features table with required columns.
#'
#' @param n_features Integer. Number of features. Default: 10
#' @param mz_range Numeric vector of length 2. m/z range. Default: c(100, 500)
#' @param rt_range Numeric vector of length 2. RT range in minutes. Default: c(0, 10)
#'
#' @return data.frame with feature_id, mz, rt columns
#'
#' @examples
#' features <- create_test_features(n_features = 100)
create_test_features <- function(
  n_features = 10L,
  mz_range = c(100, 500),
  rt_range = c(0, 10)
) {
  data.frame(
    feature_id = sprintf("FT%04d", seq_len(n_features)),
    mz = runif(n_features, mz_range[1], mz_range[2]),
    rt = runif(n_features, rt_range[1], rt_range[2]),
    stringsAsFactors = FALSE
  )
}

#' Create minimal annotations table for testing
#'
#' @description
#' Generates a simple annotations table for testing weighting and filtering.
#'
#' @param n_annotations Integer. Number of annotation rows. Default: 20
#' @param n_features Integer. Number of unique features. Default: 5
#'
#' @return data.frame with annotation columns
#'
#' @examples
#' annotations <- create_test_annotations(n_annotations = 50)
create_test_annotations <- function(
  n_annotations = 20L,
  n_features = 5L
) {
  stopifnot(n_annotations >= n_features)

  data.frame(
    feature_id = sample(
      sprintf("FT%04d", seq_len(n_features)),
      n_annotations,
      replace = TRUE
    ),
    candidate_structure_inchikey = paste0(
      replicate(
        n_annotations,
        paste(sample(LETTERS, 14, replace = TRUE), collapse = "")
      ),
      "-",
      replicate(
        n_annotations,
        paste(sample(LETTERS, 10, replace = TRUE), collapse = "")
      ),
      "-",
      replicate(n_annotations, paste(sample(LETTERS, 1), collapse = ""))
    ),
    candidate_structure_smiles = replicate(
      n_annotations,
      paste(
        sample(c("C", "O", "N", "=", "-", "(", ")"), 10, replace = TRUE),
        collapse = ""
      )
    ),
    candidate_score_similarity = runif(n_annotations, 0, 1),
    candidate_mass_error_ppm = runif(n_annotations, -10, 10),
    stringsAsFactors = FALSE
  )
}

#' Create minimal taxonomy table for testing
#'
#' @description
#' Generates a simple taxonomy table for testing biological annotations.
#'
#' @param n_taxa Integer. Number of taxa. Default: 5
#'
#' @return data.frame with organism and taxonomy columns
#'
#' @examples
#' taxonomy <- create_test_taxonomy(n_taxa = 10)
create_test_taxonomy <- function(n_taxa = 5L) {
  genera <- c("Gentiana", "Arabidopsis", "Solanum", "Coffea", "Nicotiana")
  species <- c("lutea", "thaliana", "tuberosum", "arabica", "tabacum")

  data.frame(
    organism_name = paste(
      sample(genera, n_taxa, replace = TRUE),
      sample(species, n_taxa, replace = TRUE)
    ),
    organism_taxonomy_ottid = sample(100000:999999, n_taxa),
    organism_taxonomy_01domain = "Eukaryota",
    organism_taxonomy_02kingdom = "Plantae",
    organism_taxonomy_03phylum = sample(
      c("Tracheophyta", "Chlorophyta"),
      n_taxa,
      replace = TRUE
    ),
    organism_taxonomy_06family = sample(
      c("Gentianaceae", "Brassicaceae", "Solanaceae"),
      n_taxa,
      replace = TRUE
    ),
    stringsAsFactors = FALSE
  )
}

# ==============================================================================
# Test Data Fixtures - Integration Test Setup
# ==============================================================================

#' Setup complete test workflow with features, libraries, and metadata
#'
#' @description
#' Creates a complete test environment with all necessary files for integration
#' testing of the full annotation workflow.
#'
#' @param paths Named list. Default paths from local_test_project()
#' @param download_examples Logical. Whether to download example files. Default: FALSE
#'
#' @return Invisible NULL (files are created in the test environment)
#'
#' @examples
#' test_that("integration test", {
#'   paths <- local_test_project()
#'   setup_integration_workflow(paths)
#'   # Run integration tests
#' })
setup_integration_workflow <- function(paths, download_examples = FALSE) {
  if (download_examples) {
    # Download example files
    get_file(
      url = paths$urls$examples$features,
      export = paths$data$source$features
    )
    get_file(
      url = paths$urls$examples$metadata,
      export = paths$data$source$metadata
    )
  }

  # Create fake libraries for faster testing
  fake_lotus(export = paths$data$source$libraries$sop$lotus)
  prepare_libraries_sop_lotus()

  invisible(NULL)
}

# ==============================================================================
# Assertion Helpers
# ==============================================================================

#' Expect a valid InChIKey format
#'
#' @description
#' Tests whether a string matches the InChIKey format pattern.
#'
#' @param inchikey Character vector to test
#'
#' @examples
#' expect_valid_inchikey("LFQSCWFLJHTTHZ-UHFFFAOYSA-N")
expect_valid_inchikey <- function(inchikey) {
  pattern <- "^[A-Z]{14}-[A-Z]{10}-[A-Z]$"
  testthat::expect_match(inchikey, pattern, info = "Invalid InChIKey format")
}

#' Expect a data frame with required columns
#'
#' @description
#' Tests whether a data frame contains all required column names.
#'
#' @param df Data frame to test
#' @param required_cols Character vector of required column names
#'
#' @examples
#' expect_required_columns(df, c("feature_id", "mz", "rt"))
expect_required_columns <- function(df, required_cols) {
  testthat::expect_true(
    all(required_cols %in% colnames(df)),
    info = paste(
      "Missing required columns:",
      paste(setdiff(required_cols, colnames(df)), collapse = ", ")
    )
  )
}
