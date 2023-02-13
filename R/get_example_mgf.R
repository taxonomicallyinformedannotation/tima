#' @title Get example MGF
#'
#' @description This function gets an example mgf file to work with
#'
#' @param url URL of the MGF file to be downloaded
#' @param export File path where the MGF file should be saved
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom utils download.file
#'
#' @examples NULL
get_example_mgf <-
  function(url = paths$url$examples$mgf,
           export = paths$data$source$examples$spectra) {
    ## Set the timeout for download to 600 seconds
    options(timeout = 600)
    message("Timeout for download is ", getOption("timeout"), " seconds")

    ## Create the export directory if it does not exist
    create_dir(export = export)

    ## Download the file from the given URL and save it to the specified location
    message("Downloading")
    utils::download.file(
      url = url,
      destfile = export
    )
  }
