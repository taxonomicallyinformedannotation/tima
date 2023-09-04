#' @title Remove peaks above precursor in MS2 spectra
#'
#' @description This function remove peaks above precursor in MS2 spectra
#'
#' @details Credit goes to Carolin Huber (0000-0002-9355-8948)
#'    With fine tuning of Michele Stravs (0000-0002-1426-8572)
#'
#' @param tol_mz m/z tolerance
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
remove_above_precursor <- function(tol_mz = 0.5) {
  function(x, precursorMz, ...) {
    x[!(x[, 1] >= precursorMz - tol_mz), , drop = FALSE]
  }
}
