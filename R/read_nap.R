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
read_nap <- function(id) {
  file <-
    paste0(
      "https://proteomics2.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
      id,
      "&block=main&file=final_out/node_attributes_table.tsv"
    )
  return(readr::read_delim(file = file))
}
