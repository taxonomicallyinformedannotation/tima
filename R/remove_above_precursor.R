## Credit goes to Carolin Huber (0000-0002-9355-8948) @chufz

#' @title Remove peaks above precursor in MS2 spectra
#'
#' @description This function remove peaks above precursor in MS2 spectra
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
remove_above_precursor <- function() {
  function(x, precursorMz, ...) {
    x[!(x[, 1] >= precursorMz), , drop = FALSE]
  }
}
