#' @title Setup logger
#'
#' @description Configures the logger for the package with trace-level logging
#'     and output to both console and a log file
#'
#' @param filename Character string specifying the log file name.
#'     Default: "tima.log"
#'
#' @return NULL (invisibly). Sets up logger as side effect.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' setup_logger("analysis.log")
#' }
setup_logger <- function(filename = "tima.log") {
  # Validate filename
  if (missing(filename) || is.null(filename) || nchar(filename) == 0L) {
    stop("Log filename must be specified")
  }

  # Configure logger with TRACE level (most verbose)
  logger::log_threshold(logger::TRACE)

  # Set up appender to write to both console and file
  logger::log_appender(
    appender = logger::appender_tee(file = filename)
  )

  logger::log_debug("Logger initialized with file: ", filename)

  invisible(NULL)
}
