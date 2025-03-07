#' Wrapper for the C function "gnps"
#'
#' @param x_mat A numeric matrix.
#' @param y_mat A numeric matrix.
#' @return The result from the C function.
gnps_wrapper <- function(x_mat, y_mat) {
  .Call("gnps", x = x_mat, y = y_mat)
}

#' Wrapper for the C function "join_gnps"
#'
#' @param query_masses Numeric vector or matrix for query masses.
#' @param target_masses Numeric vector or matrix for target masses.
#' @param query_precursor Numeric vector of precursor values for queries.
#' @param target_precursor Numeric vector of precursor values for targets.
#' @param dalton Numeric value specifying the tolerance in daltons.
#' @param ppm Numeric value specifying the tolerance in ppm.
#' @return The result returned from the C function "join_gnps".
#' @export
join_gnps_wrapper <- function(query_masses, target_masses, query_precursor, target_precursor, dalton, ppm) {
  .Call(
    "join_gnps",
    x = query_masses,
    y = target_masses,
    xPrecursorMz = query_precursor,
    yPrecursorMz = target_precursor,
    tolerance = dalton,
    ppm = ppm
  )
}
