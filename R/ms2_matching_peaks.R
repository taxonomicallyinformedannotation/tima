#' @title Calculate the number of matching peaks in two spectra
#'
#' @author Courtesy of Michele Stravs ([0000-0002-1426-8572](https://orcid.org/0000-0002-1426-8572))
#'
#' @description This function calculates the absolute number of matching peaks in two spectra
#'
#' @param x a matrix with two columns: \emph{m/z} and intensity values for the first spectrum
#' @param y a matrix with two columns: \emph{m/z} and intensity values for the first spectrum
#' @param ... ...
#' @param cutoff a numeric value indicating the minimum intensity required for a peak to be considered
#'
#' @return Integer indicating the number of matching peaks between x and y
#'
#' @export
#'
#' @examples NULL
.ms2_matching_peaks <- function(x, y, ..., cutoff = 0) {
  # Select only the peaks with intensity greater than the cutoff
  x <- x[x[, 2] > cutoff, 1]
  y <- y[y[, 2] > cutoff, 1]

  # Count the number of peaks that are present in both x and y
  sum(x %in% y)
}


#' @title Calculate the fraction of matching peaks in two spectra
#'
#' @author Courtesy of Michele Stravs ([0000-0002-1426-8572](https://orcid.org/0000-0002-1426-8572))
#'
#' @description This function calculates the relative number (ratio) of matching peaks in two spectra
#'
#' @param x a matrix with two columns: m/z and intensity values for the first spectrum
#' @param ... ...
#'
#' @return Numeric value indicating the fraction of matching peaks between x and y
#'
#' @export
#'
#' @examples NULL
.ms2_matching_peaks_fraction <- function(x, ...) {
  # Calculate the number of matching peaks
  matching_peaks <- .ms2_matching_peaks(x, ...)

  # Calculate the fraction of matching peaks by dividing the number of matching peaks by the number of peaks in x
  matching_peaks / nrow(x)
}
