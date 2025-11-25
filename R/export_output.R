#' @title Export data frame to file
#'
#' @description Exports a data frame to a tab-delimited file with automatic
#'     directory creation and optional gzip compression. Provides progress
#'     logging for large datasets.
#'
#' @include create_dir.R
#' @include validators.R
#'
#' @param x Data frame or tibble to export
#' @param file Character string path to the output file. File extension
#'     determines compression (.gz for gzip compression).
#'
#' @return Character string path to the exported file (invisibly)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(id = 1:100, value = rnorm(100))
#' export_output(df, "output/data.tsv")
#' export_output(df, "output/data.tsv.gz")  # Compressed
#' unlink("output", recursive = TRUE)
#' }
export_output <- function(x, file) {
  # Input Validation ----
  validate_character(file, param_name = "file", allow_empty = FALSE)
  validate_dataframe(x, param_name = "x")

  # Prepare Export ----
  create_dir(export = file)

  # Cache dimensions
  dims <- dim(x)
  nrows <- dims[1]
  ncols <- dims[2]

  logger::log_info("Exporting data to: {file}")
  logger::log_debug("Dimensions: {nrows} rows × {ncols} columns")

  # Determine compression based on file extension
  compress_method <- if (grepl("\\.gz$", file, ignore.case = TRUE)) {
    "gzip"
  } else {
    "none"
  }

  # Warn for large datasets
  if (nrows > 100000L) {
    logger::log_debug(
      "Large dataset ({nrows} rows) - export may take time"
    )
  }

  # Write Data with Error Handling ----
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
      logger::log_info("Successfully exported {nrows} rows")
    },
    error = function(e) {
      logger::log_error("Export failed: {conditionMessage(e)}")
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
