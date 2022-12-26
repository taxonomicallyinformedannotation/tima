#' @title Read edges
#'
#' @param id a character string containing a GNPS job ID
#'
#' @return a data frame containing the network edges for the specified GNPS job
#'
#' @export
#'
#' @importFrom readr read_delim
#' @importFrom stringr str_length
#'
#' @examples edges <- read_edges("1234567890abcdef1234567890abcdef")
read_edges <- function(id) {
  # Check if the provided ID is a valid GNPS job ID
  # (32 characters long)
  stopifnot(stringr::str_length(id) == 32, "Your job ID is invalid")

  # Construct the URL for the network edges file for the specified GNPS job
  file <- paste0(
    "https://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
    id,
    "&block=main&file=networkedges_selfloop/"
  )
  # Read the network edges file and return the data as a data frame
  return(readr::read_delim(file = file))
}
