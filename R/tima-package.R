#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib tima, .registration = TRUE
## usethis namespace: end
NULL
.datatable.aware <- TRUE

.onLoad <- function(libname, pkgname) {
  ## Hack to avoid rcmdcheck warning
  clue::cl_tabulate(1)
  DT::`%>%`
  gt::`%>%`
  RcppArmadillo::armadillo_get_number_of_omp_threads()
  shinybusy::use_busy_spinner()
  shinyWidgets::animations
  visNetwork::`%>%`
  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to ", pkgname)
  packageStartupMessage(format(utils::citation(pkgname)))
}
