#' @title Read features
#'
#' @param id a character string containing a GNPS job ID
#'
#' @return a data frame containing the features for the specified GNPS job
#'
#' @export
#'
#' @importFrom readr read_delim
#' @importFrom stringr str_length
#'
#' @examples features <- read_features(id = "abcdefghijklmnopqrstuvwxyz123456")
read_features <- function(id) {
  # Check if the length of the ID is 32 characters
  stopifnot(stringr::str_length(id) == 32, "Your job ID is invalid")

  # Construct the URL for the features file
  file <- paste0(
    "https://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?",
    "task=",
    id,
    "&block=main&file=quantification_table_reformatted/"
  )

  # Read the file and return the resulting data frame
  return(readr::read_delim(file = file))
}
