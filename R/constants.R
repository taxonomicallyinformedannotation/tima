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
#' @export
ELECTRON_MASS_DALTONS <- 5.485799E-4

#' Proton mass in Daltons
#' @export
PROTON_MASS_DALTONS <- 1.007276466812

# Default Tolerance Values ----

#' Default mass tolerance in parts per million (ppm) for MS1
#' @export
DEFAULT_TOLERANCE_PPM_MS1 <- 10

#' Default mass tolerance in ppm for MS2
#' @export
DEFAULT_TOLERANCE_PPM_MS2 <- 20

#' Default retention time tolerance in minutes for adduct grouping
#' @export
DEFAULT_TOLERANCE_RT_ADDUCTS <- 0.05

#' Default retention time tolerance in minutes for library matching
#' @export
DEFAULT_TOLERANCE_RT_LIBRARY <- 0.5

# Validation Thresholds ----

#' Maximum allowed mass tolerance in ppm
#' Tolerances above this value may produce excessive false positives
#' @export
MAX_TOLERANCE_PPM <- 20

#' Maximum allowed retention time tolerance for adduct grouping (minutes)
#' @export
MAX_TOLERANCE_RT_ADDUCTS <- 0.05

#' Minimum mass value (Da) - used for validation
#' @export
MIN_MASS_DALTONS <- 0

#' Maximum practical mass value (Da) for small molecules
#' Used for sanity checking, not hard limit
#' @export
MAX_MASS_DALTONS <- 5000

# Regular Expression Patterns ----

#' Regular expression pattern for parsing adduct notation
#' Matches patterns like: \[M+H\]+, \[2M+Na\]+, \[M-H2O+H\]+, \[2M1+H\]2+
#' Groups: (1) multimer, (2) isotope, (3) modifications, (4) charge count, (5) charge sign
#' @export
ADDUCT_REGEX_PATTERN <- "\\[(\\d*)M(?![a-z])(\\d*)([+-][\\w\\d].*)?.*\\](\\d*)([+-])?"

#' Regular expression for individual adduct modifications
#' Matches: +H, -H2O, +Na, etc.
#' Groups: (1) sign, (2) count, (3) element/formula
#' @export
MODIFICATION_REGEX_PATTERN <- "([+-])(\\d*)([A-Z][a-z]?\\d*(?:[A-Z][a-z]?\\d*)*)"

# File Extension Patterns ----

#' Supported compressed file extensions
#' @export
COMPRESSED_EXTENSIONS <- c(".gz", ".zip", ".bz2")

#' Supported table file extensions
#' @export
TABLE_EXTENSIONS <- c(".tsv", ".csv", ".txt")

# Chemical Formula Component Masses ----
# Common adduct components and their exact masses

#' Exact masses of common adduct components (Daltons)
#' @export
ADDUCT_MASSES <- list(
  H = 1.007825032,
  Na = 22.98976928,
  K = 38.96370668,
  NH4 = 18.033823,
  Li = 7.0160034,
  Ca = 39.9625912,
  Mg = 23.9850417,
  Fe = 55.9349375,
  Cl = 34.9688527,
  Br = 78.9183371,
  HCOO = 44.9976542, # Formate
  CH3COO = 59.0133044, # Acetate
  CF3COO = 112.9855944 # Trifluoroacetate
)

# Ionization Modes ----

#' Valid ionization mode values
#' @export
VALID_MS_MODES <- c("pos", "neg")

# Logging Configuration ----

#' Default log level for package operations
#' @export
DEFAULT_LOG_LEVEL <- "INFO"

#' Log levels available (in order of severity)
#' @export
LOG_LEVELS <- c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")

# Performance Settings ----

#' Default batch size for parallel processing
#' @export
DEFAULT_BATCH_SIZE <- 1000

#' Default progress reporting interval (number of items)
#' @export
DEFAULT_PROGRESS_INTERVAL <- 10000

#' Default number of parallel workers (NULL = auto-detect)
#' @export
DEFAULT_NUM_WORKERS <- NULL

# Scoring Defaults ----

#' Default biological score weights by taxonomic rank
#' Lower ranks (more specific) get higher scores
#' @export
DEFAULT_BIO_SCORES <- list(
  domain = 1,
  kingdom = 2,
  phylum = 4,
  class = 8,
  order = 16,
  infraorder = 20,
  family = 32,
  subfamily = 40,
  tribe = 48,
  subtribe = 56,
  genus = 64,
  subgenus = 80,
  species = 128,
  subspecies = 256,
  variety = 512
)

#' Default chemical taxonomy score weights
#' @export
DEFAULT_CHEM_SCORES <- list(
  # ClassyFire
  cla_kingdom = 1,
  cla_superclass = 2,
  cla_class = 4,
  cla_parent = 8,
  # NPClassifier
  npc_pathway = 1,
  npc_superclass = 2,
  npc_class = 4
)

# Annotation Filtering ----

#' Default percentile for top candidate selection
#' @export
DEFAULT_BEST_PERCENTILE <- 0.9

#' Default number of neighboring candidates to retain
#' @export
DEFAULT_CANDIDATES_NEIGHBORS <- 50

#' Default number of final candidates to retain
#' @export
DEFAULT_CANDIDATES_FINAL <- 10

#' Minimum consistency score threshold
#' @export
DEFAULT_MINIMAL_CONSISTENCY <- 0.5

#' Minimum MS1 biological score threshold
#' @export
DEFAULT_MINIMAL_MS1_BIO <- 2

#' Minimum MS1 chemical score threshold
#' @export
DEFAULT_MINIMAL_MS1_CHEMO <- 2

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
    logger::log_warn(
      "Constant '",
      name,
      "' not found, using default: ",
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
      "Invalid value '",
      value,
      "'. ",
      "Must be one of: ",
      paste(constant_value, collapse = ", ")
    )
  }
  TRUE
}
