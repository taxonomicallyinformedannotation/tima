#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @useDynLib tima, .registration = TRUE
## usethis namespace: end
NULL

.datatable.aware <- TRUE

.onLoad <- function(libname, pkgname) {
  # Hints/operators to appease R CMD check and lazy loading quirks
  R.utils::Arguments()

  # NOTE: Logging is NOT initialized here to avoid creating empty log files
  # when the package is loaded. Instead, logging is initialized on-demand
  # when functions that need logging are called (e.g., run_tima()).
  # Users can still manually initialize logging with init_logging() if needed.
  # Ensure RDKit and chembl_structure_pipeline availability for Python features
  try(
    {
      reticulate::py_require(packages = c("rdkit", "chembl_structure_pipeline"))
    },
    silent = TRUE
  )

  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to ", pkgname)
  packageStartupMessage(
    utils::citation(package = pkgname) |>
      format()
  )
}
