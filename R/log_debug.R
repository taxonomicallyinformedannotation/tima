#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
log_debug <- function(...) {
  cat(paste(Sys.time(), ..., "\n"))
}