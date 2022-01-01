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
read_clusters <- function(id) {
  stopifnot("Your job ID is invalid" = stringr::str_length(id) == 32)
  file <-
    paste0(
      "https://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
      id,
      "&block=main&file=clusterinfo_summary/"
    )
  return(readr::read_delim(file = file))
}
