#' @title Read NAP
#'
#' @description TODO
#'
#' @param id a character string containing a GNPS job ID
#'
#' @return a data frame containing the node attributes table for the specified GNPS job
#'
#' @export
#'
#' @importFrom readr read_delim
#' @importFrom stringr str_length
#'
#' @examples NULL
read_nap <- function(id) {
  # Check that the ID is a valid GNPS job ID (32 characters long)
  stopifnot("Your job ID is invalid" = stringr::str_length(string = id) == 32)

  # Construct the URL for the node attributes table file
  file <- paste0(
    "https://proteomics2.ucsd.edu/ProteoSAFe/DownloadResultFile?",
    "task=",
    id,
    "&block=main",
    "&file=final_out/node_attributes_table.tsv"
  )

  # Read and return the node attributes table as a data frame
  return(readr::read_delim(file = file))
}
