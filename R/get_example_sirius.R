#' @title Get example sirius
#'
#' @param url TODO
#' @param export TODO
#'
#' @return TODO
#'
#' @export
#'
#' @examples TODO
get_example_sirius <-
  function(url = paths$url$example_sirius,
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
  }
