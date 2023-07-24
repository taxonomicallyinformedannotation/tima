utils::globalVariables(
  c(
    "ratio"
  )
)

#' @title Keep peaks
#'
#' @description This function keeps a proportion of peaks
#'
#' @param x PeakData
#' @param prop Minimal ratio to the max peak
#' @param deep Deeper sanitization
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
keep_peaks <- function(x, prop, deep) {
  if (deep) {
    x <- x > quantile(x)[1]
  }
  x > max(x, na.rm = TRUE) / prop
}
