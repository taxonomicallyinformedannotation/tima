#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @useDynLib tima, .registration = TRUE
## usethis namespace: end
NULL
.datatable.aware <- TRUE

.onLoad <- function(libname, pkgname) {
  ## Hack to avoid rcmdcheck warning
  DT::`%>%`
  gt::`%>%`
  shinybusy::use_busy_spinner()
  shinyWidgets::animations
  visNetwork::`%>%`
  reticulate::py_require("rdkit")
  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to ", pkgname)
  packageStartupMessage(format(utils::citation(pkgname)))
}
