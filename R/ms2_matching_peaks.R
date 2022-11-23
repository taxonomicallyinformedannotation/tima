#' Courtesy of Michele Stravs (ORCID = "0000-0002-1426-8572")

#' @title .ms2 matching peaks
#'
#' @noRd
#'
#' @param x TODO
#' @param y TODO
#' @param ... TODO
#' @param cutoff TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom tidyr replace_na
#'
#' @examples TODO
.ms2_matching_peaks <- function(x, y, ..., cutoff = 0) {
  sum((tidyr::replace_na(
    data = x[, 2L] > cutoff, replace = 0
  )) &
    tidyr::replace_na(data = y[, 2L] > cutoff, replace = 0))
}

#' @title .ms2 matching peaks fraction
#'
#' @noRd
#'
#' @param x TODO
#' @param ... TODO
#'
#' @return TODO
#'
#' @export
#'
#' @examples TODO
.ms2_matching_peaks_fraction <- function(x, ...) {
  .ms2_matching_peaks(x, ...) / nrow(x)
}
