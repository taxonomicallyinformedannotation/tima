#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
read_metadata <-
  function(id) {
    file <-
      paste0(
        "http://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
        id,
        "&block=main&file=metadata_table/"
      )
    return(readr::read_delim(file = file))
  }