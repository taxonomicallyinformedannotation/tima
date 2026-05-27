# Test Fixtures and Helpers ----

## Internal Utility Helpers ----

fixture_roots <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) {
      return(cache)
    }

    roots <- character()

    pkg_root <- tryCatch(
      find_package_root(),
      error = function(e) NA_character_
    )
    if (!is.na(pkg_root)) {
      roots <- c(roots, file.path(pkg_root, "tests", "testthat", "fixtures"))
    }

    testthat_root <- tryCatch(
      testthat::test_path("fixtures"),
      error = function(e) NA_character_
    )
    if (!is.na(testthat_root)) {
      roots <- c(roots, testthat_root)
    }

    roots <- c(roots, file.path("tests", "testthat", "fixtures"))

    roots <- unique(roots)
    roots <- roots[!is.na(roots)]
    roots <- normalizePath(roots, winslash = "/", mustWork = FALSE)
    cache <<- roots
    roots
  }
})

resolve_fixture_path <- function(filename, missing_msg = NULL) {
  candidates <- file.path(fixture_roots(), filename)
  existing <- candidates[file.exists(candidates)]
  if (length(existing) > 0L) {
    return(existing[[1L]])
  }
  if (is.null(missing_msg)) {
    missing_msg <- sprintf(
      "Fixture file not found: %s (looked in %s)",
      filename,
      paste(candidates, collapse = ", ")
    )
  }
  stop(missing_msg, call. = FALSE)
}

copy_fixture_to <- function(filename, dest) {
  src <- resolve_fixture_path(filename)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(src, dest, overwrite = TRUE)) {
    stop(
      sprintf("Failed to copy fixture '%s' to '%s'", filename, dest),
      call. = FALSE
    )
  }
  dest
}

.testthat_helper_state <- local({
  new.env(parent = emptyenv())
})

log_helpers_path <- file.path("R", "logging_helpers.R")
if (
  file.exists(log_helpers_path) &&
    !exists("log_info", envir = globalenv(), mode = "function")
) {
  sys.source(log_helpers_path, envir = globalenv())
}

get_test_root <- function() {
  # Prefer option set during setup; fall back to cached value or tempdir()
  root <- getOption("tima.test_root")
  if (is.character(root) && length(root) == 1L) {
    return(root)
  }
  if (exists("test_root", envir = .testthat_helper_state, inherits = FALSE)) {
    return(get("test_root", envir = .testthat_helper_state, inherits = FALSE))
  }
  default_root <- file.path(tempdir(), sprintf("tima-tests-%s", Sys.getpid()))
  assign("test_root", default_root, envir = .testthat_helper_state)
  default_root
}


get_fixture_cache_env <- function() {
  if (
    !exists("fixture_cache", envir = .testthat_helper_state, inherits = FALSE)
  ) {
    assign(
      "fixture_cache",
      new.env(parent = emptyenv()),
      envir = .testthat_helper_state
    )
  }
  get("fixture_cache", envir = .testthat_helper_state, inherits = FALSE)
}

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
  # Convert character to numeric level if needed
  level_map <- list(
    TRACE = 600,
    DEBUG = 500,
    INFO = 400,
    WARN = 300,
    ERROR = 200,
    FATAL = 100
  )
  lvl <- if (is.character(threshold)) {
    level_map[[toupper(threshold)]] %||% threshold
  } else {
    threshold
  }
  lgr::lgr$set_threshold(level = lvl)
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
  base_dir <- get_test_root()

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
  base_dir <- get_test_root()

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
#'   "names", "stereo", "structure_organism_pairs", "structure_taxonomy_cla",
#'   "structure_taxonomy_npc", "lotus", "closed"
#' @param cache Logical. Whether to cache loaded fixtures for faster re-loading.
#'   Default: TRUE
#'
#' @return tidytable containing the fixture data
#'
#' @examples
#' annotations <- load_fixture("annotations")
#' features <- load_fixture("features")
#' lotus <- load_fixture("lotus")
load_fixture <- function(fixture_name, cache = TRUE) {
  fixture_path <- resolve_fixture_path(
    paste0(fixture_name, ".csv"),
    missing_msg = paste0(
      "Fixture '",
      fixture_name,
      "' not found."
    )
  )

  # Use a simple caching mechanism
  cache_env <- get_fixture_cache_env()

  if (cache && exists(fixture_name, envir = cache_env, inherits = FALSE)) {
    return(get(fixture_name, envir = cache_env, inherits = FALSE))
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

  resolve_fixture_path(
    fixture_name,
    missing_msg = paste0(
      "MGF fixture '",
      fixture_name,
      "' not found."
    )
  )
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
  repeat {
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
    "structure_taxonomy_cla",
    "structure_taxonomy_npc"
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



## Extended Fixtures ----


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
      adducts = list(pos = "[M+H]+", neg = "[M-H]-"),
      tolerances = list(mass = list(ppm = list(ms1 = 5, ms2 = 10))),
      neutral_losses = "H2O"
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

prepare_annotation_fixture_env <- function(
  feature_fixture,
  library_fixture,
  dir_root = temp_test_dir("annotate_masses"),
  feature_dest = "features.tsv"
) {
  dirs <- list(
    root = dir_root,
    features = file.path(dir_root, "data", "interim", "features"),
    libraries = file.path(dir_root, "data", "interim", "libraries"),
    outputs = file.path(dir_root, "data", "interim")
  )
  lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)

  feature_path <- file.path(dirs$features, feature_dest)
  copy_csv_fixture_to_dir(feature_fixture, dirs$features, feature_dest)

  library_path <- file.path(dirs$libraries, "library.tsv")
  copy_csv_fixture_to_dir(library_fixture, dirs$libraries, "library.tsv")

  structures <- list(
    stereo = "structures_stereo.csv",
    metadata = "structures_metadata.csv",
    names = "structures_names.csv",
    tax_cla = "structures_taxonomy_cla.csv",
    tax_npc = "structures_taxonomy_npc.csv"
  )

  structure_paths <- lapply(names(structures), function(name) {
    dest_name <- paste0(name, ".tsv")
    copy_csv_fixture_to_dir(structures[[name]], dirs$libraries, dest_name)
    file.path(dirs$libraries, dest_name)
  })
  names(structure_paths) <- names(structures)

  list(
    dirs = dirs,
    features = feature_path,
    library = library_path,
    str_stereo = structure_paths$stereo,
    str_met = structure_paths$metadata,
    str_tax_cla = structure_paths$tax_cla,
    str_tax_npc = structure_paths$tax_npc,
    output_annotations = file.path(dirs$outputs, "annotations.tsv"),
    output_edges = file.path(dirs$outputs, "edges.tsv")
  )
}

copy_csv_fixture_to_dir <- function(fixture, dir, dest) {
  src <- if (grepl("\\.csv$", fixture)) fixture else paste0(fixture, ".csv")
  copy_fixture_to(src, file.path(dir, dest))
}
