if (!require(readr)) {
  install.packages("readr")
  require(package = "readr", quietly = TRUE)
}

#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
read_edges <- function(id) {
  file <-
    paste0(
      "http://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
      id,
      "&block=main&file=networkedges_selfloop/"
    )
  return(readr::read_delim(file = file))
}
