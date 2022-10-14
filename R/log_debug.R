#' @title Log debug
#'
#' @noRd
#'
#' @param ... TODO
#'
#' @return TODO
#'
#' @export
#'
#' @examples TODO
log_debug <- function(...) {
  cat(paste(Sys.time(), ..., "\n"))
}
