#' @title Log pipe
#'
#' @description Simple helper for debugging between pipes
#'
#' @param x value for the pipe
#' @param ... one or more values to be logged
#'
#' @return Message for debugging
#'
#' @examples NULL
log_pipe <- function(x, ...) {
  x2 <- x
  log_debug(...)
  return(x2)
}
