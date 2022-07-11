if (!require(readr)) {
  install.packages("readr")
  require(package = "readr", quietly = TRUE)
}

if (!require(stringr)) {
  install.packages("stringr")
  require(package = "stringr", quietly = TRUE)
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
#' @importFrom readr read_delim
#' @importFrom stringr str_length
#'
#' @examples
read_features <- function(id) {
  stopifnot("Your job ID is invalid" = stringr::str_length(id) == 32)

  file <-
    paste0(
      "https://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
      id,
      "&block=main&file=quantification_table_reformatted/"
    )
  return(readr::read_delim(file = file))
}
