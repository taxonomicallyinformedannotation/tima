##########################   Functions - features   ###########################

# require(RCurl)
require(package = readr,quietly = TRUE)

# download_featuretable <- function(id, path = "feature_table.csv") {
#   #file = paste0("http://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",id,"&block=main&file=cluster_buckets/") #Old buckettable format
#   file = paste(
#     "http://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
#     id,
#     "&block=main&file=quantification_table_reformatted/",
#     sep = ""
#   )
#   download_file(file, path)
# }

###############################################################################

# download_metadatatable <-
#   function(id, path = "metadata_table.csv") {
#     file = paste(
#       "http://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
#       id,
#       "&block=main&file=metadata_table/",
#       sep = ""
#     )
#     download_file(file, path)
#   }

###############################################################################

#' #' Title
#' #'
#' #' @param file
#' #' @param path
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' download_file <- function(file, path = "NULL") {
#'   if (is.null(path)) {
#'     error("Must enter a path")
#'   }
#'   f <- RCurl::CFILE(path, mode = "wb")
#'   RCurl::curlPerform(url = file, writedata = f@ref)
#'   close(f)
#' }

###############################################################################

# download_cytoscapedata <- function(id, path = "buckettable.tsv") {
#   #file = paste0("http://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",id,"&block=main&file=cluster_buckets/") #Old buckettable format
#   file = paste(
#     "http://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
#     id,
#     "&block=main&file=quantification_table_reformatted/",
#     sep = ""
#   )
#   download_file(file, path)
# }

###############################################################################

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

###############################################################################

#' Title
#'
#' @param id
#'
#' @return
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

###############################################################################

#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
read_results <- function(id) {
  file <-
    paste0(
      "http://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
      id,
      "&block=main&file=DB_result/"
    )
  return(readr::read_delim(
    file = file,
  ))
}

###############################################################################

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

###############################################################################

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

###############################################################################

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

###############################################################################
