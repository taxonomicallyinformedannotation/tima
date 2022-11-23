#' @title Get example MGF
#'
#' @param url TODO
#' @param export TODO
#'
#' @return TODO
#'
#' @export
#'
#' @examples TODO
get_example_mgf <-
  function(url = paths$url$example_mgf,
           export = paths$data$source$example$mgf) {
    options(timeout = 600)
    message("Timeout for download is ", getOption("timeout"), " seconds")

    paths <- parse_yaml_paths()

    create_dir(export = export)

    message("Downloading")
    utils::download.file(
      url = url,
      destfile = export
    )
  }
