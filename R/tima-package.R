#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @useDynLib tima, .registration = TRUE
## usethis namespace: end
NULL

.datatable.aware <- TRUE

.onLoad <- function(libname, pkgname) {
  # NOTE: Logging is NOT initialized here to avoid creating empty log files
  # when the package is loaded. Instead, logging is initialized on-demand
  # when functions that need logging are called (e.g., run_tima()).
  # Users can still manually initialize logging with init_logging() if needed.

  # Hints/operators to appease R CMD check and lazy loading quirks
  DT::`%>%`
  gt::`%>%`
  shinybusy::use_busy_spinner()
  shinyWidgets::animations
  visNetwork::`%>%`

  # Ensure RDKit availability for Python-based features (no-op if unavailable)
  try(
    {
      reticulate::py_require(packages = "rdkit")
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
