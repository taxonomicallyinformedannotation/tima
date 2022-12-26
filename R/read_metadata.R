#' @title Read metadata
#'
#' @param id a character string containing a GNPS job ID
#'
#' @return a data frame containing the metadata for the specified GNPS job
#'
#' @export
#'
#' @importFrom readr read_delim
#' @importFrom stringr str_length
#'
#' @examples metadata <- read_metadata("abcdefghijklmnopqrstuvwxyz123456")
read_metadata <- function(id) {
  # Check if the input is a valid GNPS job ID
  stopifnot(stringr::str_length(id) == 32, "Your job ID is invalid")

  # Construct the URL for the metadata file
  file <-
    paste0(
      "https://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
      id,
      "&block=main&file=metadata_table/"
    )

  # Read and return the metadata file as a data frame
  return(readr::read_delim(file = file))
}
