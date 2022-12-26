#' @title Get example MGF
#'
#' @param url the URL of the MGF file to be downloaded
#' @param export the file path where the MGF file should be saved
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom utils download.file
#'
#' @examples TODO
get_example_mgf <-
  function(url = paths$url$example_mgf,
           export = paths$data$source$example$mgf) {
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
