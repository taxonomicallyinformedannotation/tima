#' @title Export output
#'
#' @description This function creates the output directory
#'    if it doesn't exist and exports the data frame to a tab-delimited file.
#'
#' @include create_dir.R
#'
#' @param x data frame to be exported
#' @param file path to the output file
#'
#' @return The path of the exported file
#'
#' @export
#'
#' @examples
#' \dontrun{
#' export_output(x = data.frame(), file = "output/file.tsv")
#' unlink("output", recursive = TRUE)
#' }
export_output <- function(x, file = output) {
  # ## Use default system data directory
  # file <- file.path(
  #   rappdirs::user_data_dir(
  #     appname = appname,
  #     appauthor = appauthor,
  #     version = version
  #   ),
  #   file
  # )

  ## Create the output directory if it doesn't exist
  create_dir(export = file)

  ## Log the path to the output file
  logger::log_info("... path to export is {file}")

  ## Write the data frame to a tab-delimited file
  tidytable::fwrite(
    x = x,
    file = file,
    sep = "\t",
    na = ""
  )

  ## To track the correct path later
  return(file)
}
