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

#' Proton mass in Daltons
#' @keywords internal
PROTON_MASS_DALTONS <- 1.007276466812

# Default Tolerance Values ----

#' Default mass tolerance in parts per million (ppm) for MS1
#' @keywords internal
DEFAULT_TOLERANCE_PPM_MS1 <- 10

#' Default mass tolerance in ppm for MS2
#' @keywords internal
DEFAULT_TOLERANCE_PPM_MS2 <- 10

#' Default retention time tolerance in minutes for adduct grouping
#' @keywords internal
DEFAULT_TOLERANCE_RT_ADDUCTS <- 0.02

#' Default retention time tolerance in minutes for library matching
#' @keywords internal
DEFAULT_TOLERANCE_RT_LIBRARY <- Inf

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
#' # Matches: [M+H]+, [2M+Na]+, [M-H2O+H]+, [2M1+H]2+
#' @keywords internal
ADDUCT_REGEX_PATTERN <- "\\[(\\d*)M(?![a-z])(\\d*)([+-][\\w\\d].*)?.*\\](\\d*)([+-])?"

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

# File Extension Patterns ----

#' Supported compressed file extensions
#' @keywords internal
COMPRESSED_EXTENSIONS <- c(".gz", ".zip", ".bz2")

#' Supported table file extensions
#' @keywords internal
TABLE_EXTENSIONS <- c(".tsv", ".csv", ".txt")

#' Supported spectral data formats
#' @keywords internal
SPECTRAL_FORMATS <- c(".mgf", ".msp", ".mzML", ".mzXML")

# File System Constants ----

#' Temporary file prefix for write tests
#' @keywords internal
TEMP_FILE_PREFIX <- ".tima_write_test_"

#' Maximum path length (OS-dependent, conservative estimate)
#' @keywords internal
MAX_PATH_LENGTH <- 260L

# Chemical Formula Component Masses ----
# Common adduct components and their exact masses

#' Exact masses of common adduct components (Daltons)
#' @keywords internal
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

# Statistical Thresholds ----

#' Tolerance for floating point weight sum comparisons
#' Used when validating that weights sum to 1.0
#' @keywords internal
WEIGHT_SUM_TOLERANCE <- 0.01

# MS1 Filtering Defaults ----

#' Default MS1 score combination logic for filtering
#' Either "OR" (at least one threshold met) or "AND" (both thresholds met)
#' @keywords internal
DEFAULT_MINIMAL_MS1_CONDITION <- "OR"


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

#' Default number of parallel workers (NULL = auto-detect)
#' @keywords internal
DEFAULT_NUM_WORKERS <- NULL

# Scoring Defaults ----

#' Default biological score weights by taxonomic rank
#' Lower ranks (more specific) get higher scores
#' @keywords internal
DEFAULT_BIO_SCORES <- list(
  domain = 0.1,
  kingdom = 0.2,
  phylum = 0.3,
  class = 0.4,
  order = 0.5,
  infraorder = 0.55,
  family = 0.6,
  subfamily = 0.65,
  tribe = 0.7,
  subtribe = 0.75,
  genus = 0.8,
  subgenus = 0.85,
  species = 0.9,
  subspecies = 0.95,
  variety = 1
)

#' Default chemical taxonomy score weights
#' @keywords internal
DEFAULT_CHEM_SCORES <- list(
  # ClassyFire
  cla_kingdom = 0.25,
  cla_superclass = 0.50,
  cla_class = 0.75,
  cla_parent = 1.00,
  # NPClassifier
  npc_pathway = 0.33,
  npc_superclass = 0.66,
  npc_class = 1.00
)

# Annotation Filtering ----

#' Default percentile threshold for top candidate selection
#' Only candidates with scores above this percentile are retained
#' @keywords internal
DEFAULT_BEST_PERCENTILE <- 0.9

#' Default number of neighboring candidates to retain during network annotation
#' @keywords internal
DEFAULT_CANDIDATES_NEIGHBORS <- 16

#' Default number of final candidates to report per feature
#' @keywords internal
DEFAULT_CANDIDATES_FINAL <- 1

#' Minimum consistency score threshold for taxonomic predictions
#' @keywords internal
DEFAULT_MINIMAL_CONSISTENCY <- 0.0

#' Minimum MS1 biological score threshold
#' @keywords internal
DEFAULT_MINIMAL_MS1_BIO <- 0.0

#' Minimum MS1 chemical score threshold
#' @keywords internal
DEFAULT_MINIMAL_MS1_CHEMO <- 0.0

# High-Confidence Filtering Defaults ----

#' Default minimum biological score threshold for high-confidence filter
#' @keywords internal
DEFAULT_HC_SCORE_BIO_MIN <- 0.85

#' Minimum chemical score for high confidence annotations
#' @keywords internal
DEFAULT_HC_SCORE_CHEM_MIN <- 0.8

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
DEFAULT_HC_MAX_RT_ERROR_MIN <- 0.05

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
