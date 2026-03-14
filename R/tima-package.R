#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @useDynLib tima, .registration = TRUE
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL

.datatable.aware <- TRUE

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
  cli::cli_inform(
    "Welcome to {.pkg {pkgname}} v{utils::packageVersion(pkgname)}"
  )
}
