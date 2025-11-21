# Test Fixtures and Helpers ----

## Internal Utility Helpers ----

### Deterministic random helper ----
.with_seed <- function(seed, expr) {
  if (!is.null(seed)) {
    # Initialize RNG deterministically without relying on existing .Random.seed
    set.seed(seed)
    on.exit(
      {
        # Restore a random state (not strictly needed for tests) by reseeding with current time
        set.seed(as.integer(Sys.time()) %% .Machine$integer.max)
      },
      add = TRUE
    )
  }
  force(expr)
}

### Pseudo InChIKey generator (format only, not chemically valid) ----
generate_fake_inchikey <- function(n = 1L, seed = NULL) {
  .with_seed(seed, {
    vapply(
      seq_len(n),
      function(i) {
        paste0(
          paste(sample(LETTERS, 14, TRUE), collapse = ""),
          "-",
          paste(sample(LETTERS, 10, TRUE), collapse = ""),
          "-",
          sample(LETTERS, 1)
        )
      },
      character(1)
    )
  })
}

## Environment Setup Fixtures ----

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
  # Suppress logs during tests for cleaner output
  local_quiet_logging()

  # if (isTRUE(copy)) {
  #   copy_backbone(cache_dir = ".")
  # }

  get_default_paths()
}

#' Quiet logging within a test scope
#'
#' @description
#' Suppresses verbose logging output during tests while preserving warnings
#' and errors.
#'
#' @param threshold Character or logger level. Minimum log level to display.
#'   Default: "ERROR"
#'
#' @return Invisible NULL
#'
#' @examples
#' test_that("quiet test", {
#'   local_quiet_logging("ERROR")
#'   # Only errors will be logged
#' })
local_quiet_logging <- function(threshold = "ERROR") {
  if (requireNamespace("logger", quietly = TRUE)) {
    # Convert character to logger level if needed
    lvl <- tryCatch(
      get(threshold, envir = asNamespace("logger")),
      error = function(...) threshold
    )
    logger::log_threshold(level = lvl)
  }
  invisible(NULL)
}

#' Create a temporary file path for test outputs
#'
#' @description
#' Creates a file path in the test temp directory to avoid polluting
#' the tests/testthat directory. Files created with this helper are
#' automatically cleaned up after tests complete.
#'
#' @param filename Character. Name of the file (e.g., "output.tsv")
#' @param subdir Character. Optional subdirectory within temp dir
#'
#' @return Character path to temporary file location
#'
#' @examples
#' test_that("writes to temp", {
#'   output_file <- temp_test_path("output.tsv")
#'   # Use output_file for writing
#' })
temp_test_path <- function(filename, subdir = NULL) {
  if (!exists(".test_root", envir = .GlobalEnv)) {
    # Fallback if .test_root not set
    base_dir <- file.path(tempdir(), sprintf("tima-tests-%s", Sys.getpid()))
  } else {
    base_dir <- get(".test_root", envir = .GlobalEnv)
  }

  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

  if (!is.null(subdir)) {
    full_dir <- file.path(base_dir, subdir)
    dir.create(full_dir, recursive = TRUE, showWarnings = FALSE)
    return(file.path(full_dir, filename))
  }

  file.path(base_dir, filename)
}

#' Create a temporary directory for test outputs
#'
#' @description
#' Creates a directory in the test temp directory for organizing multiple
#' test files. Automatically cleaned up after tests complete.
#'
#' @param dirname Character. Name of the directory
#'
#' @return Character path to temporary directory
#'
#' @examples
#' test_that("creates temp dir", {
#'   temp_dir <- temp_test_dir("my_test_data")
#'   # Use temp_dir for creating files
#' })
temp_test_dir <- function(dirname) {
  if (!exists(".test_root", envir = .GlobalEnv)) {
    base_dir <- file.path(tempdir(), sprintf("tima-tests-%s", Sys.getpid()))
  } else {
    base_dir <- get(".test_root", envir = .GlobalEnv)
  }

  full_path <- file.path(base_dir, dirname)
  dir.create(full_path, recursive = TRUE, showWarnings = FALSE)
  full_path
}

## Data Fixtures ----

### Spectral ----

#' Create a minimal test spectrum
#'
#' @description
#' Generates a simple mass spectrum for testing spectral processing functions.
#'
#' @param n_peaks Integer. Number of peaks in the spectrum. Default: 5
#' @param mz_range Numeric vector of length 2. m/z range for peaks. Default: c(100, 500)
#' @param intensity_range Numeric vector of length 2. Intensity range. Default: c(100, 1000)
#' @param seed Integer. Random seed for reproducibility. Default: NULL
#'
#' @return Matrix with two columns (mz, int) and n_peaks rows
#'
#' @examples
#' spectrum <- create_test_spectrum(n_peaks = 10)
create_test_spectrum <- function(
  n_peaks = 5L,
  mz_range = c(100, 500),
  intensity_range = c(100, 1000),
  seed = NULL
) {
  .with_seed(seed, {
    stopifnot(
      is.numeric(n_peaks),
      n_peaks >= 0,
      length(mz_range) == 2,
      length(intensity_range) == 2
    )
    if (n_peaks == 0) {
      return(matrix(
        numeric(0),
        ncol = 2,
        dimnames = list(NULL, c("mz", "int"))
      ))
    }
    mz <- sort(runif(n_peaks, mz_range[1], mz_range[2]))
    intensity <- runif(n_peaks, intensity_range[1], intensity_range[2])
    matrix(c(mz, intensity), ncol = 2, dimnames = list(NULL, c("mz", "int")))
  })
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
#' @param seed Integer. Random seed for reproducibility. Default: NULL
#'
#' @return List of n_similar spectrum matrices
#'
#' @examples
#' base <- create_test_spectrum()
#' similar <- create_similar_spectra(base, n_similar = 5)
create_similar_spectra <- function(
  base_spectrum,
  n_similar = 3L,
  noise_level = 0.1,
  seed = NULL
) {
  stopifnot(
    is.matrix(base_spectrum),
    ncol(base_spectrum) == 2,
    n_similar > 0,
    noise_level >= 0
  )
  .with_seed(seed, {
    lapply(seq_len(n_similar), function(i) {
      noise <- matrix(rnorm(length(base_spectrum), 0, noise_level), ncol = 2)
      pmax(base_spectrum + noise, 0)
    })
  })
}

### Features and Annotations ----

#' Create minimal features table for testing
#'
#' @description
#' Generates a simple features table with required columns.
#'
#' @param n_features Integer. Number of features. Default: 10
#' @param mz_range Numeric vector of length 2. m/z range. Default: c(100, 500)
#' @param rt_range Numeric vector of length 2. RT range in minutes. Default: c(0, 10)
#' @param include_rt Logical. If TRUE, includes a rt column with retention times.
#'   Default: TRUE
#' @param include_intensity Logical. If TRUE, includes an intensity column with random
#'   intensity values. Default: TRUE
#' @param include_adduct Logical. If TRUE, includes a column for adducts. Default: FALSE
#' @param seed Integer. Random seed for reproducibility. Default: NULL
#'
#' @return data.frame with feature_id, mz, rt columns
#'
#' @examples
#' features <- create_test_features(n_features = 100)
create_test_features <- function(
  n_features = 10L,
  mz_range = c(100, 500),
  rt_range = c(0, 10),
  include_rt = TRUE,
  include_intensity = TRUE,
  include_adduct = FALSE,
  seed = NULL
) {
  .with_seed(seed, {
    df <- tidytable::tidytable(
      feature_id = sprintf("FT%04d", seq_len(n_features)),
      mz = runif(n_features, mz_range[1], mz_range[2])
    )
    if (include_rt) {
      df$rt <- runif(n_features, rt_range[1], rt_range[2])
    }
    if (include_intensity) {
      df$intensity <- runif(n_features, 1e3, 1e5)
    }
    if (include_adduct) {
      df$adduct <- sample(c("[M+H]+", "[M+Na]+", "[M-H]-"), n_features, TRUE)
    }
    df
  })
}

#' Create minimal annotations table for testing
#'
#' @description
#' Generates a simple annotations table for testing weighting and filtering.
#'
#' @param n_annotations Integer. Number of annotation rows. Default: 20
#' @param n_features Integer. Number of unique features. Default: 5
#' @param seed Integer. Random seed for reproducibility. Default: NULL
#'
#' @return data.frame with annotation columns
#'
#' @examples
#' annotations <- create_test_annotations(n_annotations = 50)
create_test_annotations <- function(
  n_annotations = 20L,
  n_features = 5L,
  seed = NULL
) {
  stopifnot(n_annotations >= n_features)
  .with_seed(seed, {
    tidytable::tidytable(
      feature_id = sample(
        sprintf("FT%04d", seq_len(n_features)),
        n_annotations,
        TRUE
      ),
      candidate_structure_inchikey = generate_fake_inchikey(n_annotations),
      candidate_structure_smiles = replicate(
        n_annotations,
        paste(
          sample(c("C", "O", "N", "=", "-", "(", ")"), 10, TRUE),
          collapse = ""
        )
      ),
      candidate_score_similarity = runif(n_annotations, 0, 1),
      candidate_mass_error_ppm = runif(n_annotations, -10, 10)
    )
  })
}

#' Create minimal taxonomy table for testing
#'
#' @description
#' Generates a simple taxonomy table for testing biological annotations.
#'
#' @param n_taxa Integer. Number of taxa. Default: 5
#' @param seed Integer. Random seed for reproducibility. Default: NULL
#'
#' @return data.frame with organism and taxonomy columns
#'
#' @examples
#' taxonomy <- create_test_taxonomy(n_taxa = 10)
create_test_taxonomy <- function(n_taxa = 5L, seed = NULL) {
  .with_seed(seed, {
    genera <- c("Gentiana", "Arabidopsis", "Solanum", "Coffea", "Nicotiana")
    species <- c("lutea", "thaliana", "tuberosum", "arabica", "tabacum")
    tidytable::tidytable(
      organism_name = paste(
        sample(genera, n_taxa, TRUE),
        sample(species, n_taxa, TRUE)
      ),
      organism_taxonomy_ottid = sample(100000:999999, n_taxa),
      organism_taxonomy_01domain = "Eukaryota",
      organism_taxonomy_02kingdom = "Plantae",
      organism_taxonomy_03phylum = sample(
        c("Tracheophyta", "Chlorophyta"),
        n_taxa,
        TRUE
      ),
      organism_taxonomy_06family = sample(
        c("Gentianaceae", "Brassicaceae", "Solanaceae"),
        n_taxa,
        TRUE
      )
    )
  })
}

### Integration Test Setup ----

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


## Assertion Helpers ----

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
  testthat::expect_match(
    object = inchikey,
    regexp = pattern,
    info = "Invalid InChIKey format"
  )
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

## CSV Fixture Loaders ----

#' Load a test fixture from CSV
#'
#' @description
#' Centralized loader for CSV fixtures in tests/testthat/fixtures/ directory.
#' Provides caching and validation for consistent test data loading.
#'
#' @param fixture_name Character. Name of the fixture file (without .csv extension).
#'   Available: "annotations", "features", "edges", "components", "metadata",
#'   "names", "stereo", "structure_organism_pairs", "taxonomy_classyfire",
#'   "taxonomy_npc", "lotus_minimal", "closed_minimal"
#' @param cache Logical. Whether to cache loaded fixtures for faster re-loading.
#'   Default: TRUE
#'
#' @return tidytable containing the fixture data
#'
#' @examples
#' annotations <- load_fixture("annotations")
#' features <- load_fixture("features")
#' lotus <- load_fixture("lotus_minimal")
load_fixture <- function(fixture_name, cache = TRUE) {
  fixture_path <- testthat::test_path("fixtures", paste0(fixture_name, ".csv"))

  if (!file.exists(fixture_path)) {
    stop(
      "Fixture '",
      fixture_name,
      "' not found at: ",
      fixture_path,
      "\nAvailable fixtures: annotations, features, edges, components, ",
      "metadata, names, stereo, structure_organism_pairs, ",
      "taxonomy_classyfire, taxonomy_npc, lotus_minimal, closed_minimal"
    )
  }

  # Use a simple caching mechanism
  cache_env <- get_fixture_cache()

  if (cache && exists(fixture_name, envir = cache_env)) {
    return(get(fixture_name, envir = cache_env))
  }

  # Load the fixture
  fixture_data <- tidytable::fread(
    fixture_path,
    na.strings = c("", "NA"),
    encoding = "UTF-8"
  )

  # Cache it
  if (cache) {
    assign(fixture_name, fixture_data, envir = cache_env)
  }

  fixture_data
}

#' Load MGF fixture
#'
#' @description
#' Load a test MGF file from fixtures directory.
#'
#' @param fixture_name Character. Name of MGF file (with or without .mgf extension).
#'   Available: "spectra_query.mgf", "spectra_library.mgf"
#'
#' @return Character path to the fixture file (read-only)
#'
#' @examples
#' query_mgf <- load_mgf_fixture("spectra_query.mgf")
#' lib_mgf <- load_mgf_fixture("spectra_library")
load_mgf_fixture <- function(fixture_name) {
  if (!grepl("\\.mgf$", fixture_name)) {
    fixture_name <- paste0(fixture_name, ".mgf")
  }

  fixture_path <- testthat::test_path("fixtures", fixture_name)

  if (!file.exists(fixture_path)) {
    stop(
      "MGF fixture '",
      fixture_name,
      "' not found at: ",
      fixture_path,
      "\nAvailable MGF fixtures: spectra_query.mgf, spectra_library.mgf"
    )
  }

  fixture_path
}

#' Copy MGF fixture to temp location for modification
#'
#' @description
#' Copies an MGF fixture to a temporary location where it can be modified
#' during tests without affecting the original fixture.
#'
#' @param fixture_name Character. Name of MGF file
#' @param dest_name Character. Optional destination name. Default: same as source
#'
#' @return Character path to the copied file in temp directory
#'
#' @examples
#' temp_mgf <- copy_mgf_fixture("spectra_query.mgf")
copy_mgf_fixture <- function(fixture_name, dest_name = NULL) {
  if (!grepl("\\.mgf$", fixture_name)) {
    fixture_name <- paste0(fixture_name, ".mgf")
  }

  src <- load_mgf_fixture(fixture_name)

  if (is.null(dest_name)) {
    dest_name <- basename(fixture_name)
  }

  dest <- file.path(tempfile(), dest_name)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  file.copy(src, dest, overwrite = TRUE)
  dest
}

## CSV Fixture Loaders (Continued) ----

#' Get or create the fixture cache environment
#'
#' @keywords internal
get_fixture_cache <- function() {
  if (!exists(".fixture_cache", envir = .GlobalEnv)) {
    assign(".fixture_cache", new.env(parent = emptyenv()), envir = .GlobalEnv)
  }
  get(".fixture_cache", envir = .GlobalEnv)
}

#' Find package root directory
#'
#' @description
#' Finds the package root by looking for DESCRIPTION file
#'
#' @keywords internal
find_package_root <- function() {
  # Start from current directory
  current_dir <- getwd()

  # Walk up the directory tree
  while (TRUE) {
    if (file.exists(file.path(current_dir, "DESCRIPTION"))) {
      return(current_dir)
    }

    parent_dir <- dirname(current_dir)
    if (parent_dir == current_dir) {
      # Reached root without finding DESCRIPTION
      stop("Could not find package root (DESCRIPTION file)")
    }
    current_dir <- parent_dir
  }
}

#' Load all available fixtures
#'
#' @description
#' Loads all CSV fixtures into a named list for convenient access.
#'
#' @return Named list of fixtures
#'
#' @examples
#' fixtures <- load_all_fixtures()
#' features <- fixtures$features
#' annotations <- fixtures$annotations
load_all_fixtures <- function() {
  fixture_names <- c(
    "annotations",
    "features",
    "edges",
    "components",
    "metadata",
    "names",
    "stereo",
    "structure_organism_pairs",
    "taxonomy_classyfire",
    "taxonomy_npc"
  )

  fixtures <- lapply(
    stats::setNames(object = fixture_names, nm = fixture_names),
    load_fixture
  )

  fixtures
}

#' Create a test features table with edge connections
#'
#' @description
#' Creates features and edges tables that are consistent with each other.
#' Ensures edges reference existing features.
#'
#' @param n_features Integer. Number of features to create. Default: 10
#' @param edge_density Numeric. Proportion of possible edges to create (0-1).
#'   Default: 0.3
#' @param seed Integer. Random seed. Default: NULL
#'
#' @return List with 'features' and 'edges' data frames
#'
#' @examples
#' data <- create_features_with_edges(n_features = 20, edge_density = 0.2)
#' features <- data$features
#' edges <- data$edges
create_features_with_edges <- function(
  n_features = 10L,
  edge_density = 0.3,
  seed = NULL
) {
  .with_seed(seed, {
    # Create features
    features <- create_test_features(n_features = n_features, seed = seed)

    # Create edges between features
    feature_ids <- features$feature_id
    n_possible_edges <- n_features * (n_features - 1) / 2
    n_edges <- max(1L, round(n_possible_edges * edge_density))

    # Generate unique pairs
    pairs <- combn(feature_ids, 2, simplify = FALSE)
    selected_pairs <- sample(pairs, min(n_edges, length(pairs)))

    edges <- tidytable::tidytable(
      feature_source = vapply(selected_pairs, `[`, character(1), 1),
      feature_target = vapply(selected_pairs, `[`, character(1), 2),
      feature_spectrum_entropy = runif(length(selected_pairs), 0.5, 1.0),
      cosine_similarity = runif(length(selected_pairs), 0.6, 0.99)
    )

    list(
      features = features,
      edges = edges
    )
  })
}

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

## Empty Structure Generators ----

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

## Minimal Data Generators ----

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

## Extended Fixtures ----

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


# Param fixtures for prepare_params / get_params tests
# These mimic the structure expected by internal functions but are lightweight.

create_fixture_params_small <- function(id = "TESTID") {
  list(
    files = list(
      pattern = "{id}",
      features = list(raw = sprintf("features_raw_%s.tsv", id)),
      metadata = list(raw = sprintf("metadata_raw_%s.tsv", id)),
      spectral = list(raw = sprintf("spectral_raw_%s.mgf", id)),
      annotations = list(raw = list(sirius = sprintf("sirius_raw_%s", id)))
    ),
    ms = list(polarity = "pos"),
    organisms = list(taxon = "Gentiana lutea"),
    options = list(high_confidence = TRUE, summarize = FALSE)
  )
}

create_fixture_params_advanced <- function(id = "TESTID") {
  list(
    files = list(
      pattern = "{id}",
      annotations = list(
        raw = list(
          spectral = list(
            gnps = sprintf("gnps_raw_%s.mgf", id),
            spectral = sprintf("spectral_raw_%s.mgf", id)
          ),
          sirius = sprintf("sirius_raw_%s", id)
        ),
        prepared = list(
          canopus = sprintf("canopus_pre_%s.tsv", id),
          formula = sprintf("formula_pre_%s.tsv", id),
          structural = list(
            gnps = sprintf("struct_gnps_pre_%s.tsv", id),
            sirius = sprintf("struct_sirius_pre_%s.tsv", id),
            spectral = sprintf("struct_spectral_pre_%s.tsv", id)
          )
        ),
        filtered = sprintf("annotations_filtered_%s.tsv", id),
        processed = sprintf("annotations_processed_%s.tsv", id)
      ),
      features = list(
        raw = sprintf("features_raw_%s.tsv", id),
        prepared = list(tsv = sprintf("features_pre_%s.tsv", id))
      ),
      metadata = list(
        raw = sprintf("metadata_raw_%s.tsv", id),
        prepared = list(tsv = sprintf("metadata_pre_%s.tsv", id))
      ),
      spectral = list(raw = sprintf("spectral_raw_%s.mgf", id)),
      libraries = list(
        sop = list(merged = list(keys = sprintf("keys_%s.tsv", id))),
        spectral = list(
          pos = sprintf("lib_pos_%s.mgf", id),
          neg = sprintf("lib_neg_%s.mgf", id)
        )
      ),
      networks = list(
        spectral = list(
          edges = list(prepared = sprintf("edges_pre_%s.tsv", id)),
          components = list(prepared = sprintf("components_pre_%s.tsv", id))
        )
      )
    ),
    ms = list(
      adducts = list(pos = c("[M+H]+"), neg = c("[M-H]-")),
      tolerances = list(mass = list(ppm = list(ms1 = 5, ms2 = 10))),
      neutral_losses = c("H2O")
    ),
    annotations = list(
      candidates = list(
        best_percentile = 0.9,
        final = 5L,
        neighbors = 20L,
        samples = 3L
      ),
      ms1only = FALSE,
      ms2approx = TRUE,
      thresholds = list(
        consistency = 0.5,
        ms1 = list(biological = 0.1, chemical = 0.2, condition = "OR")
      )
    ),
    names = list(
      source = "feature_source",
      target = "feature_target",
      features = "feature_id"
    ),
    options = list(compounds_names = FALSE, force = TRUE, remove_ties = FALSE)
  )
}

create_fixture_yamls_params <- function(id = "TESTID") {
  list(
    yamls_params = list(
      prepare_params_advanced = create_fixture_params_advanced(id = id),
      annotate_masses = list(
        files = list(
          features = list(
            prepared = list(tsv = sprintf("feat_pre_%s.tsv", id))
          ),
          annotations = list(
            prepared = list(
              structural = list(tsv = sprintf("struct_pre_%s.tsv", id))
            )
          )
        )
      ),
      weight_annotations = list(
        annotations = list(
          candidates = list(best_percentile = 0.9, final = 5L, neighbors = 20L),
          thresholds = list(
            consistency = 0.5,
            ms1 = list(biological = 0.1, chemical = 0.2, condition = "OR")
          )
        ),
        files = list(pattern = "{id}")
      )
    )
  )
}

##  Usage Guide ----

# Use create_empty_table() when:
# - Testing input validation
# - Need proper column structure but no data
# Example: empty_annot <- create_empty_table("annotation")
# Use create_minimal_data() when:
# - Need 2-3 rows for simple logic tests
# - Testing basic functionality
# Example: features <- create_minimal_data("features", n_rows = 3)
# Use create_test_*() functions when:
# - Need programmatically generated data with optional deterministic seeding
# - Testing with varying sizes/scenarios
# Example: features <- create_test_features(n_features = 100, seed = 123)
# Example: spectrum <- create_test_spectrum(n_peaks = 20, seed = 456)
