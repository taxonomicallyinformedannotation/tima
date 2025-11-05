#' @title Setup logger
#'
#' @description Configures the logger for the package with trace-level logging
#'     and output to both console and a log file.
#'
#' @param filename Character string specifying the log file name.
#'     Default: "tima.log"
#' @param threshold Character or integer log level threshold
#'     (TRACE, DEBUG, INFO, WARN, ERROR). Default: logger::TRACE
#'
#' @return NULL (invisibly). Sets up logger as side effect.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' setup_logger("analysis.log")
#' setup_logger("analysis.log", threshold = logger::INFO)
#' }
setup_logger <- function(filename = "tima.log", threshold = logger::TRACE) {
  # Validate filename
  if (
    is.null(filename) ||
      !is.character(filename) ||
      length(filename) != 1L ||
      nchar(filename) == 0L
  ) {
    stop("Log filename must be a non-empty character string")
  }

  # Configure logger threshold
  logger::log_threshold(threshold)

  # Set up consistent log format with timestamp and level
  logger::log_layout(logger::layout_glue_generator(
    format = "[{time}] [{level}] {msg}"
  ))

  # Set up appender to write to both console and file
  logger::log_appender(
    appender = logger::appender_tee(file = filename)
  )

  # logger::log_info("Logger initialized - output: console + file: {filename}")
  # logger::log_debug("Log level threshold: {threshold}")

  invisible(NULL)
}
