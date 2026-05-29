#' @title Resolve package file paths
#'
#' @description Thin wrapper around [base::system.file()] that defaults to
#' package `tima`. Keeping this indirection allows focused tests to mock package
#' path resolution without patching base bindings.
#'
#' @param ... Path components passed to [base::system.file()].
#' @param package Package name; defaults to `"tima"`.
#'
#' @return Character vector of file paths.
#'
#' @keywords internal
pkg_system_file <- function(..., package = "tima") {
  system.file(..., package = package)
}
