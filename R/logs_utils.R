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
    test = b < KB,
    yes = paste0(b, " B"),
    no = ifelse(
      test = b < MB,
      yes = paste0(round(b / KB, 1), " KB"),
      no = ifelse(
        test = b < GB,
        yes = paste0(round(b / MB, 1), " MB"),
        no = paste0(round(b / GB, 2), " GB")
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
      test = sec == 0,
      yes = paste0(m, "m"),
      no = paste0(m, "m ", sec, "s")
    )
  }

  # >= 3600 seconds -> Xh Ym
  idx4 <- s >= 3600
  if (any(idx4)) {
    total_m <- s[idx4] %/% 60
    h <- total_m %/% 60
    m <- round(total_m - h * 60)
    out[!bad][idx4] <- ifelse(
      test = m == 0,
      yes = paste0(h, "h"),
      no = paste0(h, "h ", m, "m")
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
    appender = lgr::AppenderConsole$new(layout = custom_layout),
    name = "console"
  )
  lgr::lgr$add_appender(
    appender = lgr::AppenderFile$new(file = filename, layout = custom_layout),
    name = "file"
  )

  invisible(NULL)
}

#' @title Initialize logging from environment
#'
#' @description Convenience wrapper to configure logging based on environment
#'     variables. Safe to call multiple times (idempotent).
#'
#' @details
#' Environment variables:
#' - TIMA_LOG_FILE: Path to log file (default: "tima.log")
#' - TIMA_LOG_LEVEL: TRACE|DEBUG|INFO|WARN|ERROR (default: DEFAULT_LOG_LEVEL)
#'
#' This function is automatically called on first use of any logging function,
#' so explicit initialization is optional. However, you can call it explicitly
#' to customize logging before any messages are logged.
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

#' Check if logging has been initialized
#'
#' @return Logical. TRUE if logger has file appender configured.
#' @keywords internal
#' @noRd
is_logging_initialized <- function() {
  # Check if lgr has a file appender configured
  appenders <- lgr::lgr$appenders
  if (length(appenders) == 0) {
    return(FALSE)
  }

  # Helper to check if an appender is a file appender
  .is_file_appender <- function(x) inherits(x, "AppenderFile")

  has_file <- any(vapply(
    X = appenders,
    FUN = .is_file_appender,
    logical(1)
  ))

  return(has_file)
}

#' Ensure logging is initialized (lazy initialization)
#'
#' @description Checks if logging is initialized and initializes it if not.
#'     This allows logging to be set up only when first needed, avoiding
#'     creation of empty log files when package is merely loaded.
#'
#' @return NULL (invisibly)
#' @keywords internal
#' @noRd
ensure_logging_initialized <- function() {
  if (!is_logging_initialized()) {
    init_logging()
  }
  invisible(NULL)
}

#' @title Logging wrapper functions for lgr compatibility
#'
#' @description Simple logging wrappers that use sprintf-style formatting.
#'     These functions automatically initialize logging on first use (lazy initialization),
#'     so no empty log files are created when the package is merely loaded.
#'
#' @param msg Message template (use sprintf format: "Value: %s")
#' @param ... Values to substitute into msg via sprintf
#'
#' @return NULL (invisibly)
#' @keywords internal
#' @name logging_wrappers
NULL

#' @rdname logging_wrappers
log_trace <- function(msg, ...) {
  ensure_logging_initialized()
  if (...length() > 0) {
    msg <- sprintf(msg, ...)
  }
  lgr::lgr$trace(msg)
  invisible(NULL)
}

#' @rdname logging_wrappers
log_debug <- function(msg, ...) {
  ensure_logging_initialized()
  if (...length() > 0) {
    msg <- sprintf(msg, ...)
  }
  lgr::lgr$debug(msg)
  invisible(NULL)
}

#' @rdname logging_wrappers
log_info <- function(msg, ...) {
  ensure_logging_initialized()
  if (...length() > 0) {
    msg <- sprintf(msg, ...)
  }
  lgr::lgr$info(msg)
  invisible(NULL)
}

#' @rdname logging_wrappers
log_warn <- function(msg, ...) {
  ensure_logging_initialized()
  if (...length() > 0) {
    msg <- sprintf(msg, ...)
  }
  lgr::lgr$warn(msg)
  invisible(NULL)
}

#' @rdname logging_wrappers
log_error <- function(msg, ...) {
  ensure_logging_initialized()
  if (...length() > 0) {
    msg <- sprintf(msg, ...)
  }
  lgr::lgr$error(msg)
  invisible(NULL)
}

#' @rdname logging_wrappers
log_fatal <- function(msg, ...) {
  ensure_logging_initialized()
  if (...length() > 0) {
    msg <- sprintf(msg, ...)
  }
  lgr::lgr$fatal(msg)
  invisible(NULL)
}

#' @rdname logging_wrappers
log_success <- function(msg, ...) {
  ensure_logging_initialized()
  if (...length() > 0) {
    msg <- sprintf(msg, ...)
  }
  lgr::lgr$info(msg)
  invisible(NULL)
}

#' @keywords internal
#' @title Logging helpers
#' @description Utilities for consistent logging output.
#'
#' @details
#' log_similarity_distribution: logs the distribution of similarity scores
#' binned by 0.1 within \[0, 1\], including bins with zero counts. Output is
#' formatted using capture.output to match other table-style logs.
#'
#' @param scores Numeric vector (or coercible) of scores expected in \[0, 1\].
#'   Values outside are clamped to the range; NA/NaN/Inf are ignored.
#' @param title Character scalar for the header line preceding the table log.
#'
#' @return Invisibly returns NULL. Side-effects: logs via log_info.
#'
log_similarity_distribution <- function(scores, title) {
  sc <- suppressWarnings(as.numeric(scores))
  sc <- sc[is.finite(sc)]
  if (length(sc) == 0L) {
    return(invisible(NULL))
  }
  # Clamp to [0, 1]
  sc <- pmin(pmax(sc, 0), 1)
  bins <- cut(
    x = sc,
    breaks = seq(0, 1, by = 0.1),
    include.lowest = TRUE,
    right = TRUE
  )
  # Ensure all bins are present, including zeros
  lvl <- levels(bins)
  tab <- table(bins)
  counts <- as.integer(tab[lvl])
  dist <- data.frame(bin = lvl, N = counts, stringsAsFactors = FALSE)
  log_info(title)
  log_info(
    "\n%s",
    paste(
      utils::capture.output(print.data.frame(x = dist, row.names = FALSE)),
      collapse = "\n"
    )
  )
  invisible(NULL)
}

#' @keywords internal
#' @title Accumulate similarity score bins
#' @description Build or update 0.1-binned counts for scores in \[0,1\].
#' @param scores Numeric vector of scores (NA/Inf ignored; values clamped to \[0,1\]).
#' @return Named integer vector of counts for each bin label (all bins present).
accumulate_similarity_bins <- function(scores) {
  breaks <- seq(0, 1, by = 0.1)
  sc <- suppressWarnings(as.numeric(scores))
  sc <- sc[is.finite(sc)]
  if (length(sc) == 0L) {
    tmp <- cut(
      breaks[-length(breaks)],
      breaks = breaks,
      include.lowest = TRUE,
      right = TRUE
    )
    out <- integer(length(levels(tmp)))
    names(out) <- levels(tmp)
    return(out)
  }
  sc <- pmin(pmax(sc, 0), 1)
  bins <- cut(x = sc, breaks = breaks, include.lowest = TRUE, right = TRUE)
  lvl <- levels(bins)
  tab <- table(bins)
  counts <- as.integer(tab[lvl])
  names(counts) <- lvl
  counts
}

#' @keywords internal
#' @title Log similarity distribution from counts
#' @description Log a pre-accumulated distribution (named counts) in the standard table style.
#' @param counts Named integer vector whose names are bin labels (e.g., "(0,0.1\]", "(0.1,0.2\]", ...).
#' @param title Header line to log before the table.
#' @return Invisibly returns NULL.
log_similarity_distribution_counts <- function(counts, title) {
  if (is.null(counts) || length(counts) == 0L) {
    return(invisible(NULL))
  }
  dist <- data.frame(
    bin = names(counts),
    N = as.integer(counts),
    stringsAsFactors = FALSE
  )
  log_info(title)
  log_info(
    "\n%s",
    paste(
      utils::capture.output(print.data.frame(x = dist, row.names = FALSE)),
      collapse = "\n"
    )
  )
  invisible(NULL)
}
