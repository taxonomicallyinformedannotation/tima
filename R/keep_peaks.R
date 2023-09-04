#' @title Keep peaks
#'
#' @description This function keeps a proportion of peaks
#'
#' @param x PeakData
#' @param prop Minimal ratio to the max peak
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
keep_peaks <- function(x, prop) {
  x > max(x, na.rm = TRUE) / prop
}
