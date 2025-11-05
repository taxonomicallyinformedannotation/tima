#' @title Export output
#'
#' @description This function creates the output directory if it doesn't exist
#'     and exports a data frame to a tab-delimited file
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

  if (is.null(file) || !is.character(file) || length(file) != 1L || nchar(file) == 0L) {
    stop("Output file path must be a non-empty character string")
  }

  # Create the output directory if it doesn't exist
  create_dir(export = file)

  # Log the path to the output file
  logger::log_info("Exporting to: ", file)

  # Write the data frame to a tab-delimited file
  tidytable::fwrite(
    x = x,
    file = file,
    sep = "\t",
    na = ""
  )

  # Return the file path for pipeline tracking
  return(file)
}
