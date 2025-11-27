#' Log with count and timing information
#'
#' @param msg Message to log
#' @param n Count of items (optional)
#' @param elapsed Elapsed time in seconds (optional)
#'
#' @return NULL (invisibly)
#' @keywords internal
log_with_count <- function(msg, n = NULL, elapsed = NULL) {
  if (!is.null(n) && !is.null(elapsed)) {
    logger::log_info("{msg} ({format_count(n)} in {format_time(elapsed)})")
  } else if (!is.null(n)) {
    logger::log_info("{msg} ({format_count(n)})")
  } else if (!is.null(elapsed)) {
    logger::log_info("{msg} ({format_time(elapsed)})")
  } else {
    logger::log_info(msg)
  }
  invisible(NULL)
}

#' Log file operation with size
#'
#' @param action Action performed (e.g., "Exported", "Downloaded", "Loaded")
#' @param path File path
#' @param size_bytes File size in bytes (optional)
#' @param n_rows Number of rows (optional)
#'
#' @return NULL (invisibly)
#' @keywords internal
log_file_op <- function(action, path, size_bytes = NULL, n_rows = NULL) {
  msg <- paste0(action, ": ", basename(path))

  details <- character(0)
  if (!is.null(n_rows)) {
    details <- c(details, paste0(format_count(n_rows), " rows"))
  }
  if (!is.null(size_bytes)) {
    details <- c(details, format_bytes(size_bytes))
  }

  if (length(details) > 0) {
    msg <- paste0(msg, " (", paste(details, collapse = ", "), ")")
  }

  logger::log_info(msg)
  invisible(NULL)
}

#' Format a count with thousands separator
#'
#' @param x Numeric value
#'
#' @return Formatted string
#' @keywords internal
format_count <- function(x) {
  formatC(x = x, format = "fg", big.mark = ",", drop0trailing = TRUE)
}

#' Format file size in human-readable format
#'
#' @param bytes File size in bytes
#'
#' @return Formatted string (e.g., "1.5 MB")
#' @keywords internal
format_bytes <- function(bytes) {
  # Handle NA and negative
  out <- character(length(bytes))
  bad <- is.na(bytes) | bytes < 0
  out[bad] <- "unknown"

  # Safe default for good values
  b <- bytes[!bad]

  # Define thresholds
  KB <- 1024
  MB <- 1024^2
  GB <- 1024^3

  out[!bad] <- ifelse(
    b < KB,
    paste0(b, " B"),
    ifelse(
      b < MB,
      paste0(round(b / KB, 1), " KB"),
      ifelse(
        b < GB,
        paste0(round(b / MB, 1), " MB"),
        paste0(round(b / GB, 2), " GB")
      )
    )
  )
  out
}

#' Format elapsed time in human-readable format
#'
#' @param seconds Elapsed time in seconds
#'
#' @return Formatted string (e.g., "2.5s", "1m 30s")
#' @keywords internal
format_time <- function(seconds) {
  out <- character(length(seconds))

  # invalid inputs
  bad <- is.na(seconds) | seconds < 0
  out[bad] <- "unknown"

  s <- seconds[!bad]

  # < 1 second -> ms
  idx <- s < 1
  out[!bad][idx] <- paste0(round(s[idx] * 1000), "ms")

  # < 60 seconds -> X.Xs
  idx2 <- s >= 1 & s < 60
  out[!bad][idx2] <- paste0(round(s[idx2], 1), "s")

  # < 3600 seconds -> Xm Ys
  idx3 <- s >= 60 & s < 3600
  if (any(idx3)) {
    m <- s[idx3] %/% 60
    sec <- round(s[idx3] - m * 60)
    out[!bad][idx3] <- ifelse(
      sec == 0,
      paste0(m, "m"),
      paste0(m, "m ", sec, "s")
    )
  }

  # >= 3600 seconds -> Xh Ym
  idx4 <- s >= 3600
  if (any(idx4)) {
    total_m <- s[idx4] %/% 60
    h <- total_m %/% 60
    m <- round(total_m - h * 60)
    out[!bad][idx4] <- ifelse(
      m == 0,
      paste0(h, "h"),
      paste0(h, "h ", m, "m")
    )
  }

  out
}

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
