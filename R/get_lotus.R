#' Title
#'
#' @param url TODO
#' @param export TODO
#'
#' @return TODO
#' @export
#'
#' @examples
get_lotus <-
  function(url = paths$urls$lotus,
           export = paths$data$source$libraries$lotus) {
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

    readr::read_csv(file = curl::curl_download(url = url, destfile = tempfile())) |>
      readr::write_csv(file = export)
  }
