#' @title Read clusters
#'
#' @description This function reads the clusters table from GNPS
#'
#' @param id Character string of length 32 representing the job ID
#' @param workflow Character string indicating the type of workflow, either "fbmn" or "classical"
#'
#' @return A data frame containing cluster information
#'
#' @export
#'
#' @importFrom readr read_delim
#' @importFrom stringr str_length
#'
#' @examples NULL
read_clusters <- function(id, workflow = params$workflow) {
  # Check if the given workflow is supported
  stopifnot("Your workflow is not supported, supported workflows are 'fbmn' and 'classical'" = workflow %in% c("fbmn", "classical"))

  # Check if the given job ID is valid
  stopifnot("Your job ID is invalid" = stringr::str_length(id) == 32)

  # Construct the file URL based on the workflow type
  file <- paste0(
    "https://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
    id,
    switch(workflow,
      "fbmn" = "&block=main&file=clusterinfo_summary/",
      "classical" = "&block=main&file=clusterinfosummarygroup_attributes_withIDs_withcomponentID/"
    )
  )

  # Read and return the cluster information from the file
  return(readr::read_delim(file = file))
}
