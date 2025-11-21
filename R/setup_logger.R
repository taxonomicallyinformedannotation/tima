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
  logger::log_threshold(level = threshold)

  # Set up consistent log format with timestamp and level
  # Use POSIX formatting to force 3-digit milliseconds ("%OS3").
  # Also pad the level to a fixed width using sprintf so columns align.
  logger::log_layout(logger::layout_glue_generator(
    format = "[{format(time, \"%Y-%m-%d %H:%M:%OS3\")} ] [{sprintf(\"%-5s\", toupper(level))}] {msg}"
  ))

  # Set up appender to write to both console and file
  logger::log_appender(
    appender = logger::appender_tee(file = filename)
  )

  # logger::log_info("Logger initialized - output: console + file: {filename}")
  # logger::log_debug("Log level threshold: {threshold}")

  invisible(NULL)
}

#' @title Initialize logging from environment
#'
#' @description Convenience wrapper to configure logging based on environment
#'     variables. Safe to call multiple times.
#'
#' @details
#' Environment variables:
#' - TIMA_LOG_FILE: Path to log file (default: "tima.log")
#' - TIMA_LOG_LEVEL: TRACE|DEBUG|INFO|WARN|ERROR (default: DEFAULT_LOG_LEVEL)
#'
#' @return NULL (invisibly)
#' @keywords internal
init_logging <- function() {
  # Read env with sane defaults
  log_file <- Sys.getenv("TIMA_LOG_FILE", unset = "tima.log")
  level_env <- toupper(Sys.getenv("TIMA_LOG_LEVEL", unset = DEFAULT_LOG_LEVEL))

  # Map level string to logger constant
  level_map <- list(
    TRACE = logger::TRACE,
    DEBUG = logger::DEBUG,
    INFO = logger::INFO,
    WARN = logger::WARN,
    ERROR = logger::ERROR
  )

  threshold <- level_map[[level_env]]
  if (is.null(threshold)) {
    warning(
      sprintf(
        "Unknown TIMA_LOG_LEVEL '%s'. Falling back to %s.",
        level_env,
        DEFAULT_LOG_LEVEL
      ),
      call. = FALSE
    )
    threshold <- level_map[[DEFAULT_LOG_LEVEL]]
  }

  setup_logger(filename = log_file, threshold = threshold)
}
