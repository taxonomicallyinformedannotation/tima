## This was done because of targets re-running steps calling C code
## See https://github.com/ropensci/targets/issues/721
## Probably having them return a hash of the source C file would be even better

#' Wrapper for the C function "gnps"
#'
#' @param x A numeric matrix.
#' @param y A numeric matrix.
#' @return The result from the C function.
gnps_wrapper <- function(x, y) {
  .Call("gnps", x = x, y = y)
}

#' Wrapper for the C function "join_gnps"
#'
#' @param x Numeric vector or matrix for query masses.
#' @param y Numeric vector or matrix for target masses.
#' @param xPrecursorMz Numeric vector of precursor values for queries.
#' @param yPrecursorMz Numeric vector of precursor values for targets.
#' @param tolerance Numeric value specifying the tolerance in daltons.
#' @param ppm Numeric value specifying the tolerance in ppm.
#' @return The result returned from the C function "join_gnps".
#' @export
join_gnps_wrapper <- function(
  x,
  y,
  xPrecursorMz,
  yPrecursorMz,
  tolerance,
  ppm
) {
  .Call(
    "join_gnps",
    x = x,
    y = y,
    xPrecursorMz = xPrecursorMz,
    yPrecursorMz = yPrecursorMz,
    tolerance = tolerance,
    ppm = ppm
  )
}
