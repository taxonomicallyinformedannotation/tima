#' @title Physical and Chemical Constants
#'
#' @description This module contains centralized physical and chemical constants
#'     used throughout the TIMA package. Centralizing these values ensures
#'     consistency, facilitates maintenance, and makes the code self-documenting.
#'
#' @name constants
#' @keywords internal

# Physical Constants ----

#' Electron mass in Daltons (CODATA 2018 recommended value)
#' @keywords internal
ELECTRON_MASS_DALTONS <- 5.485799E-4

# Validation Thresholds ----

#' Maximum allowed mass tolerance in ppm
#' Tolerances above this value may produce excessive false positives
#' @keywords internal
MAX_TOLERANCE_PPM <- 20

#' Maximum allowed retention time tolerance for adduct grouping (minutes)
#' @keywords internal
MAX_TOLERANCE_RT_ADDUCTS <- 0.05

#' Minimum mass value (Da) - used for validation
#' @keywords internal
MIN_MASS_DALTONS <- 0

#' Maximum practical mass value (Da) for small molecules
#' Used for sanity checking, not hard limit
#' @keywords internal
MAX_MASS_DALTONS <- 5000

# Regular Expression Patterns ----

#' Regular expression pattern for parsing adduct notation
#'
#' @details Pattern breakdown:
#' \describe{
#'   \item{\code{\\[}}{Opening bracket (literal)}
#'   \item{\code{(\\d*)}}{Group 1: Optional multimer count (e.g., "2" in \[2M+H\]+)}
#'   \item{\code{M}}{Required literal "M" for molecule}
#'   \item{\code{(?![a-z])}}{Negative lookahead: prevent matching element symbols like "Mg"}
#'   \item{\code{(\\d*)}}{Group 2: Optional isotope shift (e.g., "1" in \[M1+H\]+ for M+1)}
#'   \item{\code{([+-][\\w\\d].*)?}}{Group 3: Optional modifications (e.g., "+H", "-H2O+Na")}
#'   \item{\code{.*\\]}}{Any characters until closing bracket}
#'   \item{\code{(\\d*)}}{Group 4: Optional charge count (e.g., "2" in \[M+H\]2+)}
#'   \item{\code{([+-])?}}{Group 5: Optional charge sign (+ or -)}
#' }
#'
#' @examples
#' # Matches: [M+H]+, [2M+Na]+, [M-H2O+H]+, [2M1+H]2+, [M+1+H]+, [M-2+H]+
#' @keywords internal
ADDUCT_REGEX_PATTERN <- "\\[(\\d*)M(?![a-z])((?:[+-]?\\d+(?![A-Z]))?)([+-][\\w\\d].*)?.*\\](\\d*)([+-])?"

#' Regular expression for individual adduct modifications
#'
#' @details Pattern breakdown:
#' \describe{
#'   \item{\code{([+-])}}{Group 1: Addition (+) or loss (-) sign}
#'   \item{\code{(\\d*)}}{Group 2: Optional stoichiometric coefficient (e.g., "2" in +2H)}
#'   \item{\code{([A-Z][a-z]?\\d*(?:[A-Z][a-z]?\\d*)*)}}{Group 3: Element formula (e.g., "H", "H2O", "NaCl")}
#' }
#'
#' @examples
#' # Matches: +H, -H2O, +2Na, +C6H12O6
#' @keywords internal
MODIFICATION_REGEX_PATTERN <- "([+-])(\\d*)([A-Z][a-z]?\\d*(?:[A-Z][a-z]?\\d*)*)"

# File System Constants ----

#' Temporary file prefix for write tests
#' @keywords internal
TEMP_FILE_PREFIX <- ".tima_write_test_"

#' Maximum path length (OS-dependent, conservative estimate)
#' @keywords internal
MAX_PATH_LENGTH <- 260L

# Statistical Thresholds ----

#' Tolerance for floating point weight sum comparisons
#' Used when validating that weights sum to 1.0
#' @keywords internal
WEIGHT_SUM_TOLERANCE <- 0.01

# Ionization Modes ----

#' Valid ionization mode values
#' @keywords internal
VALID_MS_MODES <- c("pos", "neg")

# Similarity Methods ----

#' Valid similarity method values
#' @keywords internal
VALID_SIMILARITY_METHODS <- c("entropy", "cosine", "gnps")

# Logging Configuration ----

#' Default log level for package operations
#' @keywords internal
DEFAULT_LOG_LEVEL <- "INFO"

#' Log levels available (in order of severity)
#' @keywords internal
LOG_LEVELS <- c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")

# Performance Settings ----

#' Default batch size for parallel processing
#' @keywords internal
DEFAULT_BATCH_SIZE <- 1000

#' Default progress reporting interval (number of items)
#' @keywords internal
DEFAULT_PROGRESS_INTERVAL <- 10000

# High-Confidence Filtering Defaults ----

#' Default minimum biological score threshold for high-confidence filter
#' @keywords internal
DEFAULT_HC_SCORE_BIO_MIN <- 0.85

#' Default minimum initial (pseudo) score threshold for high-confidence filter
#' @keywords internal
DEFAULT_HC_SCORE_INITIAL_MIN <- 0.95

#' Default minimum final (weighted chemical) score threshold for high-confidence filter
#' @keywords internal
DEFAULT_HC_SCORE_FINAL_MIN <- 0.75

#' Default minimum SIRIUS confidence score threshold for high-confidence filter
#' @keywords internal
DEFAULT_HC_SCORE_SIRIUS_MIN <- NULL

#' Default minimum spectral similarity score threshold for high-confidence filter
#' @keywords internal
DEFAULT_HC_SCORE_SPECTRAL_MIN <- NULL

#' Default maximum allowed retention time error (minutes) for high-confidence filter
#' @keywords internal
DEFAULT_HC_MAX_RT_ERROR_MIN <- 0.15

# InChI Patterns ----

#' InChI stereochemistry layer pattern indicating no stereochemistry
#' Structures with "-UHFFFAOYSA-" in their InChIKey have no defined stereochemistry
#' @keywords internal
INCHI_NO_STEREO_PATTERN <- "-UHFFFAOYSA-"

# Helper Functions ----

#' Get a constant value by name with validation
#'
#' @param name Character string name of the constant
#' @param default Default value to return if constant not found
#'
#' @return The constant value
#'
#' @keywords internal
get_constant <- function(name, default = NULL) {
  if (!exists(name, mode = "any")) {
    if (is.null(default)) {
      stop("Constant '", name, "' not found and no default provided")
    }
    log_warn(
      "Constant %s not found, using default: %s",
      name,
      default
    )
    return(default)
  }
  get(name)
}

#' Validate that a value matches a constant
#'
#' @param value The value to validate
#' @param constant_name Name of the constant to compare against
#'
#' @return TRUE if valid, stops with error otherwise
#'
#' @keywords internal
validate_against_constant <- function(value, constant_name) {
  constant_value <- get_constant(constant_name)
  if (!value %in% constant_value) {
    stop(
      "Invalid value %s. Must be one of: %s",
      value,
      paste(constant_value, collapse = ", ")
    )
  }
  TRUE
}
