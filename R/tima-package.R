#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
.datatable.aware <- TRUE

.onLoad <- function(libname, pkgname) {
  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to Taxonomically Informed Metabolite Annotation")
}
