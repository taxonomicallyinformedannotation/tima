#' @title Log debug
#'
#' @description Simple helper for debugging
#'
#' @export
#'
#' @param ... one or more values to be logged
#'
#' @return Message for debugging
#'
#' @examples log_debug("This is a debug message")
log_debug <- function(...) {
  ## Concatenate the current time, the specified values,
  ## and a newline character and print it
  cat(paste(format(Sys.time()), ..., "\n"))
}
