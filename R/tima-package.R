#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @useDynLib tima, .registration = TRUE
## usethis namespace: end
NULL

.datatable.aware <- TRUE

.onLoad <- function(libname, pkgname) {
  # Initialize logging from environment (safe to call multiple times)
  try({ init_logging() }, silent = TRUE)

  # Hints/operators to appease R CMD check and lazy loading quirks
  DT::`%>%`
  gt::`%>%`
  shinybusy::use_busy_spinner()
  shinyWidgets::animations
  visNetwork::`%>%`

  # Ensure RDKit availability for Python-based features (no-op if unavailable)
  try({ reticulate::py_require(packages = "rdkit") }, silent = TRUE)

  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to ", pkgname)
  packageStartupMessage(
    utils::citation(package = pkgname) |>
      format()
  )
}
