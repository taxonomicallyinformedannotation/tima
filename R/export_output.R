#' @title Export output
#'
#' @description This function creates the output directory if it doesn't exist and exports the data frame to a tab-delimited file.
#'
#' @param x data frame to be exported
#' @param file path to the output file
#' @param ... ...
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom crayon green
#' @importFrom readr write_delim
#'
#' @examples export_output(x = data.frame(), file = "output/file.tsv")
export_output <- function(x, file = output, ...) {
  # Create the output directory if it doesn't exist
  create_dir(export = file)

  # Log the path to the output file
  log_debug(
    x = "... path to export is",
    crayon::green(file)
  )

  # Write the data frame to a tab-delimited file
  readr::write_delim(
    x = x,
    file = file,
    delim = "\t",
    na = ""
  )
}
