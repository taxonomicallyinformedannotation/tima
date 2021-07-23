#' Title
#'
#' @param x
#' @param n
#'
#' @return
#' @export
#'
#' @examples
homemadeShift <- function(x, n) {
  c(x[-(seq(n))], rep(NA, n))
}