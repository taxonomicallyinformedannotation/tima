#' Title
#'
#' @param url TODO
#' @param export TODO
#'
#' @return TODO
#' @export
#'
#' @examples
get_example_sirius <-
  function(url = paths$url$example_sirius,
           export = paths$data$interim$annotations$example_sirius) {
    options(timeout = 600)
    message("Timeout for download is ", getOption("timeout"), " seconds")

    paths <- parse_yaml_paths()

    ifelse(
      test = !dir.exists(dirname(dirname(export))),
      yes = dir.create(dirname(dirname(export))),
      no = paste(
        dirname(dirname(export)),
        "exists"
      )
    )
    ifelse(
      test = !dir.exists(dirname(export)),
      yes = dir.create(dirname(export)),
      no = paste(
        dirname(export),
        "exists"
      )
    )

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
