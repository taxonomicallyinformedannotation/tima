#' @title Download file
#'
#' @description This function download files
#'
#' @param url URL of the file to be downloaded
#' @param export File path where the file should be saved
#' @param limit Timeout limit (in seconds)
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom utils download.file
#'
#' @examples download_file(url = "https://github.com/taxonomicallyinformedannotation/tima-example-files/raw/main/metadata.tsv", export = "data/source/examples/metadata.tsv")
download_file <-
  function(url,
           export,
           limit = 600) {
    ## Set the timeout for download to 600 seconds
    options(timeout = limit)
    message("Timeout for download is ", getOption("timeout"), " seconds")

    ## Create the export directory if it does not exist
    create_dir(export = export)

    ## Download the file from the given URL and save it to the specified location
    message("Downloading")
    utils::download.file(
      url = url,
      destfile = export
    )

    return(export)
  }
