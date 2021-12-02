#' Title
#'
#' @noRd
#'
#' @param x TODO
#' @param n TODO
#'
#' @return TODO
#' @export
#'
#' @examples TODO
homemadeShift <- function(x, n) {
  c(x[-(seq(n))], rep(NA, n))
}
