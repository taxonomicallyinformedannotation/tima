#' @title Get example sirius
#'
#' @description This function gets an example of Sirius results
#'
#' @param url URL of the example sirius file to be downloaded
#' @param export File path where the downloaded sirius file should be saved
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom utils download.file unzip
#'
#' @examples NULL
get_example_sirius <-
  function(url = paths$url$examples$sirius,
           export = paths$data$interim$annotations$example_sirius) {
    options(timeout = 600)
    message("Timeout for download is ", getOption("timeout"), " seconds")

    paths <- parse_yaml_paths()

    create_dir(export = export)

    message("Downloading")
    utils::download.file(
      url = url,
      destfile = export
    )
    message("Unzipping")
    utils::unzip(
      zipfile = export,
      exdir = dirname(export)
    )

    return(export)
  }
