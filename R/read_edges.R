#' @title Read edges
#'
#' @description This function reads the edges table from GNPS
#'
#' @param id Character string containing a GNPS job ID
#'
#' @return Data frame containing the network edges for the specified GNPS job
#'
#' @export
#'
#' @importFrom readr read_delim
#' @importFrom stringr str_length
#'
#' @examples NULL
read_edges <- function(id) {
  # Check if the provided ID is a valid GNPS job ID
  # (32 characters long)
  stopifnot("Your job ID is invalid" = stringr::str_length(string = id) == 32)

  # Construct the URL for the network edges file for the specified GNPS job
  file <- paste0(
    "https://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
    id,
    "&block=main&file=networkedges_selfloop/"
  )
  # Read the network edges file and return the data as a data frame
  return(readr::read_delim(file = file))
}
