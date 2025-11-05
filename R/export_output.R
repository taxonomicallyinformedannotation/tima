#' @title Export output
#'
#' @description This function creates the output directory if it doesn't exist
#'     and exports a data frame to a tab-delimited file.
#'
#' @include create_dir.R
#'
#' @param x Data frame to be exported
#' @param file Character string path to the output file
#'
#' @return The path to the exported file
#'
#' @export
#'
#' @examples
#' \dontrun{
#' export_output(x = data.frame(), file = "output/file.tsv")
#' unlink("output", recursive = TRUE)
#' }
export_output <- function(x, file) {
  # Validate input
  if (!is.data.frame(x) && !inherits(x, "tbl")) {
    stop("Input 'x' must be a data frame or tibble")
  }

  if (
    is.null(file) ||
      !is.character(file) ||
      length(file) != 1L ||
      nchar(file) == 0L
  ) {
    stop("Output file path must be a non-empty character string")
  }

  # Create the output directory if it doesn't exist
  create_dir(export = file)

  # Log export operation with data dimensions
  nrows <- nrow(x)
  ncols <- ncol(x)
  logger::log_info("Exporting data to: {file}")
  logger::log_debug(
    "Dimensions: {nrows} rows x {ncols} columns ({ncols} variables)"
  )

  # Determine if compression is needed
  is_compressed <- grepl("\\.gz$", file, ignore.case = TRUE)

  if (nrows > 100000L) {
    logger::log_debug("Large dataset detected, export may take some time...")
  }

  # Write the data frame to a tab-delimited file
  tryCatch(
    {
      tidytable::fwrite(
        x = x,
        file = file,
        sep = "\t",
        na = "",
        compress = if (is_compressed) "gzip" else "none",
        showProgress = FALSE
      )
      logger::log_info("Successfully exported {nrows} rows to {file}")
    },
    error = function(e) {
      logger::log_error("Failed to export data: {conditionMessage(e)}")
      stop("Failed to export data to ", file, ": ", conditionMessage(e))
    }
  )

  # Return the file path for pipeline tracking
  return(file)
}
