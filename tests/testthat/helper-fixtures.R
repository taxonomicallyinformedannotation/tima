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

#' Fixture Utilities
#'
#' @description
#' Utilities for loading CSV fixtures and creating minimal test data structures.
#' This file provides both CSV loaders and programmatic fixture generators.

# ==============================================================================
# CSV Fixture Loaders
# ==============================================================================

#' Load fixture CSV file
#'
#' @param filename Name of CSV file in fixtures/ directory
#' @return tidytable loaded from CSV
#' @keywords internal
load_fixture_csv <- function(filename) {
  path <- testthat::test_path("fixtures", filename)
  if (!file.exists(path)) {
    stop("Fixture file not found: ", filename)
  }
  tidytable::fread(path, na.strings = c("", "NA"))
}

#' Load structure metadata fixture
#'
#' @return tidytable with structure metadata
#' @export
#' @keywords internal
load_fixture_metadata <- function() {
  load_fixture_csv("metadata.csv")
}

#' Load structure stereochemistry fixture
#'
#' @return tidytable with stereo data
#' @export
#' @keywords internal
load_fixture_stereo <- function() {
  load_fixture_csv("stereo.csv")
}

#' Load structure names fixture
#'
#' @return tidytable with structure names
#' @export
#' @keywords internal
load_fixture_names <- function() {
  load_fixture_csv("names.csv")
}

#' Load Classyfire taxonomy fixture
#'
#' @return tidytable with Classyfire taxonomy
#' @export
#' @keywords internal
load_fixture_taxonomy_classyfire <- function() {
  load_fixture_csv("taxonomy_classyfire.csv")
}

#' Load NPClassifier taxonomy fixture
#'
#' @return tidytable with NPC taxonomy
#' @export
#' @keywords internal
load_fixture_taxonomy_npc <- function() {
  load_fixture_csv("taxonomy_npc.csv")
}

#' Load features fixture
#'
#' @return tidytable with features data
#' @export
#' @keywords internal
load_fixture_features <- function() {
  load_fixture_csv("features.csv")
}

#' Load edges fixture
#'
#' @return tidytable with network edges
#' @export
#' @keywords internal
load_fixture_edges <- function() {
  load_fixture_csv("edges.csv")
}

#' Load components fixture
#'
#' @return tidytable with network components
#' @export
#' @keywords internal
load_fixture_components <- function() {
  load_fixture_csv("components.csv")
}

#' Load annotations fixture
#'
#' @return tidytable with annotation data
#' @export
#' @keywords internal
load_fixture_annotations <- function() {
  load_fixture_csv("annotations.csv")
}

#' Load structure-organism pairs fixture
#'
#' @return tidytable with structure-organism pairs
#' @export
#' @keywords internal
load_fixture_sop <- function() {
  load_fixture_csv("structure_organism_pairs.csv")
}

# ==============================================================================
# Empty Structure Generators (for validation tests)
# ==============================================================================

#' Create empty table with required columns (for validation tests)
#'
#' @description
#' Creates a 0-row table with the correct column structure.
#' Use this for input validation tests where you need the structure but no data.
#'
#' @param template Character name of template ("annotation", "features", "sop", etc.)
#' @return Empty tidytable with correct columns
#' @export
#' @keywords internal
create_empty_table <- function(template = "annotation") {
  templates <- list(
    annotation = tidytable::tidytable(
      candidate_structure_inchikey_connectivity_layer = character(0),
      sample_organism_name = character(0),
      feature_id = character(0),
      candidate_structure_smiles_no_stereo = character(0)
    ),
    features = tidytable::tidytable(
      feature_id = character(0),
      mz = numeric(0),
      rt = numeric(0)
    ),
    sop = tidytable::tidytable(
      structure_inchikey_connectivity_layer = character(0),
      organism_name = character(0),
      organism_taxonomy_ottid = character(0),
      organism_taxonomy_01domain = character(0),
      organism_taxonomy_02kingdom = character(0),
      organism_taxonomy_03phylum = character(0),
      organism_taxonomy_04class = character(0),
      organism_taxonomy_05order = character(0),
      organism_taxonomy_06family = character(0),
      organism_taxonomy_07tribe = character(0),
      organism_taxonomy_08genus = character(0),
      organism_taxonomy_09species = character(0),
      organism_taxonomy_10varietas = character(0)
    ),
    library_keys = tidytable::tidytable(
      structure_inchikey_connectivity_layer = character(0),
      structure_smiles = character(0),
      structure_exact_mass = numeric(0),
      organism_name = character(0)
    ),
    components = tidytable::tidytable(
      feature_id = character(0),
      component_id = integer(0)
    ),
    edges = tidytable::tidytable(
      feature_source = character(0),
      feature_target = character(0),
      candidate_score_similarity = numeric(0)
    ),
    metadata = tidytable::tidytable(
      sample_organism_name = character(0),
      organism_taxonomy_ottid = character(0)
    )
  )

  if (!template %in% names(templates)) {
    stop(
      "Unknown template: ",
      template,
      ". Available: ",
      paste(names(templates), collapse = ", ")
    )
  }

  templates[[template]]
}

# ==============================================================================
# Minimal Data Generators (for simple tests)
# ==============================================================================

#' Create minimal sample data for testing
#'
#' @description
#' Extends empty templates with 2-3 rows of sample data.
#' Use this for simple logic tests where you need minimal realistic data.
#'
#' @param template Character name of template
#' @param n_rows Number of rows (default: 2)
#' @return tidytable with sample data
#' @export
#' @keywords internal
create_minimal_data <- function(template = "annotation", n_rows = 2) {
  empty <- create_empty_table(template)

  if (template == "annotation") {
    return(tidytable::tidytable(
      candidate_structure_inchikey_connectivity_layer = replicate(
        n_rows,
        paste(sample(LETTERS, 14, replace = TRUE), collapse = "")
      ),
      sample_organism_name = rep("Gentiana lutea", n_rows),
      feature_id = paste0("FT", sprintf("%04d", seq_len(n_rows))),
      candidate_structure_smiles_no_stereo = rep("CCCC", n_rows)
    ))
  }

  if (template == "features") {
    return(tidytable::tidytable(
      feature_id = paste0("FT", sprintf("%04d", seq_len(n_rows))),
      mz = runif(n_rows, 100, 500),
      rt = runif(n_rows, 0.1, 10.0)
    ))
  }

  if (template == "sop") {
    return(create_structure_organism_pairs(n_rows))
  }

  # For other templates, return empty structure
  # (can be extended as needed)
  empty
}

# ==============================================================================
# Extended Fixtures (programmatic generation)
# ==============================================================================

#' Create structure-organism pairs with full taxonomy
#'
#' @description
#' Programmatically creates structure-organism pairs.
#' Use this when you need specific test scenarios with taxonomy.
#'
#' @param n_rows Number of rows
#' @return tidytable with complete SOP structure
#' @export
#' @keywords internal
create_structure_organism_pairs <- function(n_rows = 0) {
  result <- create_empty_table("sop")

  if (n_rows == 0) {
    return(result)
  }

  # Populate with sample data
  result <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = replicate(
      n_rows,
      paste(sample(LETTERS, 14, replace = TRUE), collapse = "")
    ),
    organism_name = sample(
      c("Gentiana lutea", "Arabidopsis thaliana", "Solanum tuberosum"),
      n_rows,
      replace = TRUE
    ),
    organism_taxonomy_ottid = as.character(sample(100000:999999, n_rows)),
    organism_taxonomy_01domain = rep("Eukaryota", n_rows),
    organism_taxonomy_02kingdom = rep("Plantae", n_rows),
    organism_taxonomy_03phylum = sample(
      c("Tracheophyta", "Chlorophyta"),
      n_rows,
      replace = TRUE
    ),
    organism_taxonomy_04class = rep("Magnoliopsida", n_rows),
    organism_taxonomy_05order = sample(
      c("Gentianales", "Brassicales", "Solanales"),
      n_rows,
      replace = TRUE
    ),
    organism_taxonomy_06family = sample(
      c("Gentianaceae", "Brassicaceae", "Solanaceae"),
      n_rows,
      replace = TRUE
    ),
    organism_taxonomy_07tribe = sample(
      c("Gentianeae", "Arabideae", "Solaneae", NA_character_),
      n_rows,
      replace = TRUE
    ),
    organism_taxonomy_08genus = sample(
      c("Gentiana", "Arabidopsis", "Solanum"),
      n_rows,
      replace = TRUE
    ),
    organism_taxonomy_09species = sample(
      c("lutea", "thaliana", "tuberosum"),
      n_rows,
      replace = TRUE
    ),
    organism_taxonomy_10varietas = rep(NA_character_, n_rows)
  )

  result
}

#' Create annotation table with taxonomy (for weight_bio tests)
#'
#' @param n_rows Number of rows
#' @return tidytable with annotation and taxonomy
#' @export
#' @keywords internal
create_annotation_table_taxed <- function(n_rows = 0) {
  base <- create_minimal_data("annotation", n_rows)

  if (n_rows == 0) {
    # Return empty structure with taxonomy columns
    taxonomy_cols <- c(
      "sample_organism_01_domain",
      "sample_organism_02_kingdom",
      "sample_organism_03_phylum",
      "sample_organism_04_class",
      "sample_organism_05_order",
      "sample_organism_06_family",
      "sample_organism_07_tribe",
      "sample_organism_08_genus",
      "sample_organism_09_species",
      "sample_organism_10_varietas"
    )
    for (col in taxonomy_cols) {
      base[[col]] <- character(0)
    }
    return(base)
  }

  # Add taxonomy for sample data
  base$sample_organism_01_domain <- "Eukaryota"
  base$sample_organism_02_kingdom <- "Plantae"
  base$sample_organism_03_phylum <- "Tracheophyta"
  base$sample_organism_04_class <- "Magnoliopsida"
  base$sample_organism_05_order <- "Gentianales"
  base$sample_organism_06_family <- "Gentianaceae"
  base$sample_organism_07_tribe <- "Gentianeae"
  base$sample_organism_08_genus <- "Gentiana"
  base$sample_organism_09_species <- "lutea"
  base$sample_organism_10_varietas <- NA_character_

  base
}

# ==============================================================================
# Usage Guide
# ==============================================================================

# Use CSV fixtures when:
# - You need realistic reference data
# - Multiple tests use the same data
# - Data is static and doesn't need to vary
# Example: metadata <- load_fixture_metadata()

# Use create_empty_table() when:
# - Testing input validation
# - Need proper column structure but no data
# Example: empty_annot <- create_empty_table("annotation")

# Use create_minimal_data() when:
# - Need 2-3 rows for simple logic tests
# - Testing basic functionality
# Example: features <- create_minimal_data("features", n_rows = 3)

# Use create_*() functions when:
# - Need programmatically generated data
# - Testing with varying sizes/scenarios
# - Need specific test conditions
# Example: sop <- create_structure_organism_pairs(n_rows = 10)
