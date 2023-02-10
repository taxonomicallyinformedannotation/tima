#' @title Read results
#'
#' @description This function reads GNPS results table
#'
#' @param id Character string containing the job ID of the GNPS task
#' @param workflow Character string indicating the type of workflow used in the GNPS task. Supported values are 'fbmn' and 'classical'
#'
#' @return Data frame containing the results of the GNPS task
#'
#' @export
#'
#' @importFrom readr read_delim
#' @importFrom stringr str_length
#'
#' @examples NULL
read_results <- function(id, workflow = params$gnps$workflow) {
  # Check that the workflow parameter is either 'fbmn' or 'classical'
  stopifnot(
    "Your workflow is not supported, supported workflows are 'fbmn' and 'classical'" = workflow %in% c("fbmn", "classical")
  )

  # Check that the id parameter is a 32 character string
  stopifnot("Your job ID is invalid" = stringr::str_length(id) == 32)

  # Construct the URL for downloading the results file
  file <-
    paste0(
      "https://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
      id,
      switch(workflow,
        "fbmn" = "&block=main&file=DB_result/",
        "classical" = "&block=main&file=result_specnets_DB/"
      )
    )

  # Read the results file from the URL and return it as a data frame
  return(readr::read_delim(file = file))
}
