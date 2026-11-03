#' @concept EDAM:topic_3172 Metabolomics
#' @concept EDAM:topic_0154 Small molecules
#' @concept EDAM:topic_2258 Cheminformatics
#' @concept EDAM:topic_0637 Taxonomy
#' @concept EDAM:topic_3370 Analytical chemistry
#' @concept EDAM:topic_0769 Workflows
#' @concept EDAM:operation_3803 Natural product identification
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @useDynLib tima, .registration = TRUE
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL

.datatable.aware <- TRUE # nolint: object_name_linter. datatable convention.

.onLoad <- function(libname, pkgname) {
  # NOTE: Logging is NOT initialized here to avoid creating empty log files
  # when the package is loaded. Instead, logging is initialized on-demand
  # when functions that need logging are called (e.g., run_tima()).
  # Users can still manually initialize logging with init_logging() if needed.

  # NOTE: Python/RDKit initialization is deferred to first use
  # (e.g., process_smiles()) to avoid slow package loading and failures
  # when Python is not installed. See load_python_smiles_processor().

  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    sprintf("Welcome to %s v%s", pkgname, utils::packageVersion(pkgname))
  )
}
