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
read_features <- function(id) {
  file <-
    paste0(
      "http://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
      id,
      "&block=main&file=quantification_table_reformatted/"
    )
  return(readr::read_delim(file = file))
}
