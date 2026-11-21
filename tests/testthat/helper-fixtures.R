# Test Fixtures and Helpers ----

## Internal Utility Helpers ----

fixture_roots <- local({
  state <- new.env(parent = emptyenv())
  state$cache <- NULL
  function() {
    if (!is.null(state$cache)) {
      return(state$cache)
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
    state$cache <- roots
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

## Assertion Helpers ----

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
    options = list(high_evidence = TRUE, summarize = FALSE)
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
      clusters = c(
        "CH2O2",
        "H4ClN",
        "NaCl",
        "CH5NO2",
        "CHNaO2",
        "ClK",
        "CHKO2",
        "C2HF3O2"
      ),
      solvents = c("H2O", "C2H3N"),
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
        ms = list(clusters = c("CH2O2"), solvents = c("H2O")),
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
