#' @title Export data frame to file
#'
#' @description Exports a data frame to a tab-delimited file with automatic
#'     directory creation and optional gzip compression. Provides progress
#'     logging for large datasets.
#'
#' @include create_dir.R
#' @include logs_utils.R
#' @include validations_utils.R
#'
#' @param x Data frame or tibble to export
#' @param file Character string path to the output file. File extension
#'     determines compression (.gz for gzip compression).
#'
#' @return Character string path to the exported file (invisibly)
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' df <- data.frame(id = 1:100, value = rnorm(100))
#' export_output(df, "output/data.tsv")
#' export_output(df, "output/data.tsv.gz") # Compressed
#' unlink("output", recursive = TRUE)
#' }
export_output <- function(x, file) {
  # Input Validation ----
  validate_character(file, param_name = "file", allow_empty = FALSE)
  validate_dataframe(x, param_name = "x")

  # Prepare Export ----
  create_dir(export = file)

  # Cache dimensions for logging
  nrows <- nrow(x)
  ncols <- ncol(x)

  log_debug(
    "Exporting %s rows x %d columns to: %s",
    format_count(nrows),
    ncols,
    basename(file)
  )

  # Determine compression
  compress_method <- if (grepl("\\.gz$", file, ignore.case = TRUE)) {
    "gzip"
  } else {
    "none"
  }

  # Warn for large datasets
  if (nrows > 100000L) {
    log_debug("Large dataset - export may take time")
  }

  # Write Data with Error Handling ----
  start_time <- Sys.time()

  tryCatch(
    {
      tidytable::fwrite(
        x = x,
        file = file,
        sep = "\t",
        na = "",
        compress = compress_method,
        showProgress = FALSE
      )

      # Log success with timing
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      file_size <- file.info(file)$size
      log_file_op("Exported", file, size_bytes = file_size, n_rows = nrows)
      log_debug("Export completed in %s", format_time(elapsed))
    },
    error = function(e) {
      log_error("Export failed: %s", conditionMessage(e))
      stop(
        "Failed to export data to '",
        file,
        "': ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  invisible(file)
}
