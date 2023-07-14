## Credit goes to Carolin Huber (0000-0002-9355-8948) @chufz
## With fine tuning of Michele Stravs (0000-0002-1426-8572) @meowcat

#' @title Remove peaks above precursor in MS2 spectra
#'
#' @description This function remove peaks above precursor in MS2 spectra
#'
#' @param tol_mz m/z tolerance
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
remove_above_precursor <- function(tol_mz = 0.5) {
  function(x, precursor_mz, ...) {
    x[!(x[, 1] >= precursor_mz - tol_mz), , drop = FALSE]
  }
}
