#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
.datatable.aware <- TRUE

.onLoad <- function(libname, pkgname) {
  Sys.setenv(TAR_CONFIG = system.file("pipelines/_targets.yaml", package = "tima"))
  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to Taxonomically Informed Metabolite Annotation")
}
