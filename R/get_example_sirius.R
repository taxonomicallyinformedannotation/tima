options(timeout = 600)

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
    paths <- parse_yaml_paths()

    ifelse(
      test = !dir.exists(dirname(dirname(export))),
      yes = dir.create(dirname(dirname(export))),
      no = paste(dirname(dirname(export)),
                 "exists")
    )
    ifelse(
      test = !dir.exists(dirname(export)),
      yes = dir.create(dirname(export)),
      no = paste(dirname(export),
                 "exists")
    )

    message("Downloading")
    download.file(url = url,
                  destfile = export)
    message("Unzipping")
    unzip(zipfile = export,
          exdir = dirname(export))
  }
