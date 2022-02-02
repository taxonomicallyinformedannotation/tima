if (!require(readr)) {
  install.packages("readr")
  require(package = "readr", quietly = TRUE)
}

#' Title
#'
#' @noRd
#'
#' @param id TODO
#' @param workflow TODO
#'
#' @return TODO
#' @export
#'
#' @examples
read_results <- function(id, workflow = params$workflow) {
  stopifnot("Your workflow is not supported, supported workflows are 'fbmn' and 'classical'" = workflow %in% c("fbmn", "classical"))
  stopifnot("Your job ID is invalid" = stringr::str_length(id) == 32)

  file <-
    paste0(
      "https://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
      id,
      switch(workflow,
        "fbmn" = "&block=main&file=DB_result/",
        "classical" = "&block=main&file=result_specnets_DB/"
      )
    )
  return(readr::read_delim(file = file))
}
