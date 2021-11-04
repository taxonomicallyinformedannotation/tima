##########################   Functions - features   ###########################

# require(RCurl)
require(package = readr, quietly = TRUE)

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
