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
read_nap <- function(id) {
  stopifnot("Your job ID is invalid" = stringr::str_length(id) == 32)

  file <-
    paste0(
      "https://proteomics2.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
      id,
      "&block=main&file=final_out/node_attributes_table.tsv"
    )
  return(readr::read_delim(file = file))
}
