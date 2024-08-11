#' @title Log debug
#'
#' @description Simple helper for debugging
#'
#' @noRd
#'
#' @param ... one or more values to be logged
#'
#' @return Message for debugging
#'
#' @examples tima:::log_debug("This is a debug message")
log_debug <- function(...) {
  ## Concatenate the current time, the specified values,
  ## and a newline character and print it
  cat(paste(Sys.time(), ..., "\n"))
}
