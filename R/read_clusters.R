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
read_clusters <- function(id) {
  file <-
    paste0(
      "http://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
      id,
      "&block=main&file=clusterinfo_summary/"
    )
  return(readr::read_delim(file = file))
}
