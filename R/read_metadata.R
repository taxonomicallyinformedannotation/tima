if (!require(readr)) {
  install.packages("readr")
  require(package = "readr", quietly = TRUE)
}

#' Title
#'
#' @noRd
#'
#' @param id TODO
#'
#' @return TODO
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
