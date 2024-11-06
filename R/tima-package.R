#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
.datatable.aware <- TRUE

.onLoad <- function(libname, pkgname) {
  ## Hack to avoid rcmdcheck warning since they are needed by {targets}
  ## for shinylive
  DT:::.packageName
  gt:::.packageName
  shinybusy:::.packageName
  shinyWidgets:::.packageName
  visNetwork:::.packageName
  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to  ", pkgname)
  message(format(utils::citation(pkgname)))
}
