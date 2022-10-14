#' @title Read metadata
#'
#' @noRd
#'
#' @param id TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom readr read_delim
#' @importFrom stringr str_length
#'
#' @examples TODO
read_metadata <-
  function(id) {
    stopifnot("Your job ID is invalid" = stringr::str_length(id) == 32)

    file <-
      paste0(
        "https://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
        id,
        "&block=main&file=metadata_table/"
      )
    return(readr::read_delim(file = file))
  }
