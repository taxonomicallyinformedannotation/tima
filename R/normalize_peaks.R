#' @title Normalize peaks
#'
#' @description This function normalizes peaks
#'
#' @param x PeakData
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
normalize_peaks <- function(x) {
  x[, 2] <- x[, 2] / max(x[, 2])
  return(x)
}
