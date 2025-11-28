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
    lgr::lgr$info(sprintf(
      "%s (%s in %s)",
      msg,
      format_count(n),
      format_time(elapsed)
    ))
  } else if (!is.null(n)) {
    lgr::lgr$info(sprintf("%s (%s)", msg, format_count(n)))
  } else if (!is.null(elapsed)) {
    lgr::lgr$info(sprintf("%s (%s)", msg, format_time(elapsed)))
  } else {
    lgr::lgr$info(msg)
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

  lgr::lgr$info(msg)
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
#' @param threshold Character or integer log level threshold.
#'     Can be numeric (lgr levels) or logger constants for backwards compatibility.
#'     Default: 600 (TRACE level)
#'
#' @return NULL (invisibly). Sets up logger as side effect.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' setup_logger("analysis.log")
#' setup_logger("analysis.log", threshold = 400)
#' }
setup_logger <- function(filename = "tima.log", threshold = 600) {
  # Validate filename
  if (
    is.null(filename) ||
      !is.character(filename) ||
      length(filename) != 1L ||
      nchar(filename) == 0L
  ) {
    stop("Log filename must be a non-empty character string")
  }

  # Convert logger constants to lgr numeric levels if needed
  # Both logger and lgr use: TRACE=600, DEBUG=500, INFO=400, WARN=300, ERROR=200, FATAL=100
  # lgr also accepts string names: "trace", "debug", "info", "warn", "error", "fatal"
  # We'll keep the same numeric values for compatibility
  lgr_threshold <- if (is.numeric(threshold)) {
    as.integer(threshold)
  } else if (is.character(threshold)) {
    tolower(threshold)
  } else {
    stop("threshold must be numeric or character")
  }

  # Configure logger threshold
  lgr::lgr$set_threshold(lgr_threshold)

  # Create a custom layout that matches the logger format
  # Use LayoutGlue which supports glue-style formatting similar to logger
  custom_layout <- lgr::LayoutGlue$new(
    fmt = "[{format(timestamp, '%Y-%m-%d %H:%M:%OS3')}] [{pad_right(toupper(level_name), 5)}] {msg}"
  )

  # Clear any existing appenders first (lgr has a console appender by default)
  lgr::lgr$set_appenders(list())

  # Set up appenders to write to both console and file
  lgr::lgr$add_appender(
    lgr::AppenderConsole$new(layout = custom_layout),
    name = "console"
  )
  lgr::lgr$add_appender(
    lgr::AppenderFile$new(file = filename, layout = custom_layout),
    name = "file"
  )

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

  # Map level string to lgr numeric levels (same as logger)
  # TRACE=600, DEBUG=500, INFO=400, WARN=300, ERROR=200, FATAL=100
  level_map <- list(
    TRACE = 600,
    DEBUG = 500,
    INFO = 400,
    WARN = 300,
    ERROR = 200,
    FATAL = 100
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

#' @keywords internal
log_trace <- function(...) {
  msg <- eval_glue_safely(...)
  lgr::lgr$trace(msg)
  invisible(NULL)
}

#' @keywords internal
log_debug <- function(...) {
  msg <- eval_glue_safely(...)
  lgr::lgr$debug(msg)
  invisible(NULL)
}

#' @keywords internal
log_info <- function(...) {
  msg <- eval_glue_safely(...)
  lgr::lgr$info(msg)
  invisible(NULL)
}

#' @keywords internal
log_warn <- function(...) {
  msg <- eval_glue_safely(...)
  lgr::lgr$warn(msg)
  invisible(NULL)
}

#' @keywords internal
log_error <- function(...) {
  msg <- eval_glue_safely(...)
  lgr::lgr$error(msg)
  invisible(NULL)
}

#' @keywords internal
log_fatal <- function(...) {
  msg <- eval_glue_safely(...)
  lgr::lgr$fatal(msg)
  invisible(NULL)
}

#' @keywords internal
log_success <- function(...) {
  msg <- eval_glue_safely(...)
  lgr::lgr$info(msg)
  invisible(NULL)
}

#' Evaluate glue-style formatting safely
#'
#' @param ... Message parts to combine
#'
#' @return Character string with glue-style interpolation applied
#' @keywords internal
eval_glue_safely <- function(...) {
  # Collect all arguments
  args <- list(...)

  # If single character argument, process it for glue-style formatting
  if (length(args) == 1 && is.character(args[[1]])) {
    msg <- args[[1]]

    # Check if message contains glue-style {variable} patterns
    if (grepl("\\{.+?\\}", msg)) {
      # Get parent environment for variable lookup
      parent_env <- parent.frame(n = 2)

      # Simple regex-based replacement for {var} patterns
      # Extract all {var} patterns
      matches <- gregexpr("\\{([^}]+)\\}", msg)
      if (matches[[1]][1] != -1) {
        # Process each match
        match_data <- regmatches(msg, matches)[[1]]
        for (match in match_data) {
          # Extract variable name (remove { and })
          var_expr <- gsub("^\\{|\\}$", "", match)

          # Try to evaluate the expression in parent environment
          tryCatch(
            {
              value <- eval(parse(text = var_expr), envir = parent_env)
              # Replace {var} with value
              msg <- sub(match, as.character(value), msg, fixed = TRUE)
            },
            error = function(e) {
              # If evaluation fails, leave the pattern as-is
            }
          )
        }
      }
    }
    return(msg)
  }

  # Otherwise, just paste all arguments together
  paste(..., sep = "", collapse = "")
}
