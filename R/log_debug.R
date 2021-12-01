#' Title
#'
#' @param ... TODO
#'
#' @return TODO
#' @export
#'
#' @examples
log_debug <- function(...) {
  cat(paste(Sys.time(), ..., "\n"))
}
