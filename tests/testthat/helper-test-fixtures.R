#' Test Fixtures and Helpers for TIMA Tests
#'
#' This file provides reusable test fixtures, mock data generators,
#' and helper functions to improve test reliability and reduce duplication.

# Test Data Generators ----

#' Create minimal valid feature table for testing
#'
#' @param n_features Number of features to generate
#' @param include_rt Include retention time column
#' @param include_adduct Include adduct column
#' @return tidytable with feature data
#' @keywords internal
create_test_features <- function(
  n_features = 10,
  include_rt = TRUE,
  include_adduct = FALSE
) {
  features <- tidytable::tidytable(
    "feature_id" = paste0("FT", sprintf("%04d", seq_len(n_features))),
    "mz" = runif(n_features, 100, 500),
    "intensity" = runif(n_features, 1000, 100000)
  )

  if (include_rt) {
    features$rt <- runif(n_features, 0.1, 10.0)
  }

  if (include_adduct) {
    features$adduct <- sample(
      c("[M+H]+", "[M+Na]+", "[M+K]+", "[M-H]-"),
      n_features,
      replace = TRUE
    )
  }

  features
}

#' Create minimal spectral library for testing
#'
#' @param n_compounds Number of compounds to generate
#' @param ms_mode Ionization mode ("pos" or "neg")
#' @return tidytable with spectral library data
#' @keywords internal
create_test_library <- function(n_compounds = 20, ms_mode = "pos") {
  adducts <- if (ms_mode == "pos") {
    c("[M+H]+", "[M+Na]+", "[M+K]+", "[M+NH4]+")
  } else {
    c("[M-H]-", "[M+Cl]-", "[M+FA-H]-")
  }

  tidytable::tidytable(
    "structure_inchikey" = paste0(
      "FAKE",
      sprintf("%015d", seq_len(n_compounds))
    ),
    "structure_smiles" = replicate(
      n_compounds,
      paste0(
        "C",
        paste0(sample(c("C", "O", "N"), 10, replace = TRUE), collapse = "")
      )
    ),
    "structure_exact_mass" = runif(n_compounds, 100, 600),
    "structure_molecular_formula" = replicate(
      n_compounds,
      paste0(
        "C",
        sample(10:30, 1),
        "H",
        sample(10:50, 1),
        "O",
        sample(1:10, 1)
      )
    ),
    "adduct" = sample(adducts, n_compounds, replace = TRUE),
    "organism_name" = sample(
      c("Arabidopsis thaliana", "Solanum lycopersicum", "Oryza sativa"),
      n_compounds,
      replace = TRUE
    ),
    "organism_taxonomy_ottid" = sample(1000000:9999999, n_compounds)
  )
}

#' Create minimal test spectra
#'
#' @param n_spectra Number of spectra to generate
#' @param n_peaks_range Range of peaks per spectrum
#' @return List of spectral matrices
#' @keywords internal
create_test_spectra <- function(n_spectra = 5, n_peaks_range = c(5, 20)) {
  lapply(seq_len(n_spectra), function(i) {
    n_peaks <- sample(n_peaks_range[1]:n_peaks_range[2], 1)
    mz_values <- sort(runif(n_peaks, 50, 500))
    int_values <- runif(n_peaks, 100, 10000)

    matrix(
      c(mz_values, int_values),
      ncol = 2,
      dimnames = list(NULL, c("mz", "int"))
    )
  })
}

# Test Environment Setup ----

#' Setup isolated test environment with temp directory
#'
#' @param test_name Name of test (for directory naming)
#' @return Path to temporary test directory
#' @keywords internal
setup_test_env <- function(test_name = "test") {
  # Create unique temp directory
  temp_dir <- file.path(
    tempdir(),
    paste0("tima_test_", test_name, "_", Sys.getpid())
  )
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

  # Set as working directory
  old_wd <- getwd()
  setwd(temp_dir)

  # Store cleanup info
  withr::defer(
    {
      setwd(old_wd)
      unlink(temp_dir, recursive = TRUE)
    },
    envir = parent.frame()
  )

  temp_dir
}

#' Setup minimal TIMA directory structure for testing
#'
#' @param base_path Base directory path
#' @keywords internal
setup_tima_structure <- function(base_path = ".") {
  dirs <- c(
    file.path(base_path, "data/source"),
    file.path(base_path, "data/interim/features"),
    file.path(base_path, "data/interim/annotations"),
    file.path(base_path, "data/interim/libraries/sop"),
    file.path(base_path, "data/interim/libraries/spectra"),
    file.path(base_path, "data/processed")
  )

  lapply(dirs, function(d) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  })

  invisible(TRUE)
}

# Validation Helpers ----

#' Check if data frame has required columns
#'
#' @param df Data frame to check
#' @param required_cols Character vector of required column names
#' @param test_name Name of test (for error messages)
#' @keywords internal
expect_has_columns <- function(df, required_cols, test_name = "test") {
  testthat::expect_true(
    all(required_cols %in% colnames(df)),
    info = paste0(
      test_name,
      ": Missing required columns. ",
      "Expected: ",
      paste(required_cols, collapse = ", "),
      ". ",
      "Found: ",
      paste(colnames(df), collapse = ", ")
    )
  )
}

#' Check if numeric values are within expected range
#'
#' @param values Numeric vector to check
#' @param min_val Minimum expected value
#' @param max_val Maximum expected value
#' @param value_name Name of values (for error messages)
#' @keywords internal
expect_values_in_range <- function(
  values,
  min_val,
  max_val,
  value_name = "values"
) {
  testthat::expect_true(
    all(values >= min_val & values <= max_val, na.rm = TRUE),
    info = paste0(
      value_name,
      " should be between ",
      min_val,
      " and ",
      max_val,
      ". ",
      "Found range: ",
      min(values, na.rm = TRUE),
      " to ",
      max(values, na.rm = TRUE)
    )
  )
}

# Mock Data Writers ----

#' Write minimal feature file for testing
#'
#' @param path Output file path
#' @param n_features Number of features
#' @param ... Additional arguments passed to create_test_features
#' @keywords internal
write_test_features <- function(path, n_features = 10, ...) {
  features <- create_test_features(n_features = n_features, ...)

  # Ensure directory exists
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  # Write based on file extension
  if (grepl("\\.gz$", path)) {
    tidytable::fwrite(features, gsub("\\.gz$", "", path))
    R.utils::gzip(gsub("\\.gz$", "", path), destname = path, remove = TRUE)
  } else {
    tidytable::fwrite(features, path)
  }

  invisible(path)
}

#' Write minimal library file for testing
#'
#' @param path Output file path
#' @param n_compounds Number of compounds
#' @param ... Additional arguments passed to create_test_library
#' @keywords internal
write_test_library <- function(path, n_compounds = 20, ...) {
  library_data <- create_test_library(n_compounds = n_compounds, ...)

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (grepl("\\.gz$", path)) {
    tidytable::fwrite(library_data, gsub("\\.gz$", "", path))
    R.utils::gzip(gsub("\\.gz$", "", path), destname = path, remove = TRUE)
  } else {
    tidytable::fwrite(library_data, path)
  }

  invisible(path)
}

# Cleanup Helpers ----

#' Clean up test files and directories
#'
#' @param paths Character vector of paths to remove
#' @keywords internal
cleanup_test_files <- function(paths) {
  for (path in paths) {
    if (file.exists(path)) {
      if (dir.exists(path)) {
        unlink(path, recursive = TRUE)
      } else {
        file.remove(path)
      }
    }
  }
  invisible(TRUE)
}
