##########################   Functions - features   ###########################

require(RCurl)

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

#' Title
#'
#' @param file
#' @param path
#'
#' @return
#' @export
#'
#' @examples
download_file <- function(file, path = "NULL") {
  if (is.null(path)) {
    error("Must enter a path")
  }
  f <- RCurl::CFILE(path, mode = "wb")
  RCurl::curlPerform(url = file, writedata = f@ref)
  close(f)
}

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
read_featuretable <- function(id) {
  file <-
    paste0(
      "http://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
      id,
      "&block=main&file=quantification_table_reformatted/"
    )
  return(read.csv(file = file))
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
read_metadatatable <-
  function(id) {
    file <-
      paste0(
        "http://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
        id,
        "&block=main&file=metadata_table/"
      )
    return(read.csv(
      file = file,
      sep = "\t"
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
read_clusterinfo <- function(id) {
  file <-
    paste0(
      "http://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
      id,
      "&block=main&file=clusterinfo_summary/"
    )
  return(read.csv(
    file = file,
    sep = "\t"
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
read_nap <- function(id) {
  file <-
    paste0(
      "https://proteomics2.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
      id,
      "&block=main&file=final_out/node_attributes_table.tsv"
    )
  return(read.csv(
    file = file,
    sep = "\t"
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
read_edges <- function(id) {
  file <-
    paste0(
      "http://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
      id,
      "&block=main&file=networkedges_selfloop/"
    )
  return(read.csv(
    file = file,
    sep = "\t"
  ))
}

###############################################################################
# This function should export the results table as a formatted .graphml file

# require(igraph)

#' Title
#'
#' @param graphML
#' @param final
#'
#' @return
#' @export
#'
#' @examples
make_classyfire_graphml <- function(graphML, final) {
  finalordered <-
    final[match(igraph::vertex_attr(graphML, "id"), final$`feature_id`), ]

  for (i in seq_len(ncol(final))) {
    att <- colnames(finalordered)[i]
    igraph::vertex_attr(graphML, att) <- finalordered[, i]
  }

  return(graphML)
}

###############################################################################
