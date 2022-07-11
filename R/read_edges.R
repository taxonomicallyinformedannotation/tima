#' Title
#'
#' @noRd
#'
#' @param id TODO
#'
#' @return TODO
#' @export
#'
#' @importFrom readr read_delim
#' @importFrom stringr str_length
#'
#' @examples
read_edges <- function(id) {
  stopifnot("Your job ID is invalid" = stringr::str_length(id) == 32)

  file <-
    paste0(
      "https://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
      id,
      "&block=main&file=networkedges_selfloop/"
    )
  return(readr::read_delim(file = file))
}
