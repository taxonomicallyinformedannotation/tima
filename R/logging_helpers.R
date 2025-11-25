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
  prettyNum(as.character(x), big.mark = ",")
}

#' Format file size in human-readable format
#'
#' @param bytes File size in bytes
#'
#' @return Formatted string (e.g., "1.5 MB")
#' @keywords internal
format_bytes <- function(bytes) {
  if (is.na(bytes) || bytes < 0) {
    return("unknown")
  }

  if (bytes < 1024) {
    paste0(bytes, " B")
  } else if (bytes < 1024^2) {
    paste0(round(bytes / 1024, 1), " KB")
  } else if (bytes < 1024^3) {
    paste0(round(bytes / 1024^2, 1), " MB")
  } else {
    paste0(round(bytes / 1024^3, 2), " GB")
  }
}

#' Format elapsed time in human-readable format
#'
#' @param seconds Elapsed time in seconds
#'
#' @return Formatted string (e.g., "2.5s", "1m 30s")
#' @keywords internal
format_time <- function(seconds) {
  if (is.na(seconds) || seconds < 0) return("unknown")

  if (seconds < 1) 
    return(paste0(round(seconds * 1000), "ms"))

  if (seconds < 60)
    return(paste0(round(seconds, 1), "s"))

  # one division block only
  mins <- seconds %/% 60
  secs <- seconds - mins * 60

  if (seconds < 3600) {
    secs <- round(secs)
    if (secs == 0) return(paste0(mins, "m"))
    return(paste0(mins, "m ", secs, "s"))
  }

  # hours
  hrs  <- mins %/% 60
  mins <- mins - hrs * 60
  mins <- round(mins)

  if (mins == 0) return(paste0(hrs, "h"))
  paste0(hrs, "h ", mins, "m")
}

#' Format percentage
#'
#' @param x Numeric value between 0 and 1
#' @param digits Number of decimal places
#'
#' @return Formatted string (e.g., "75.5%")
#' @keywords internal
format_percent <- function(x, digits = 1) {
  paste0(round(x * 100, digits), "%")
}
