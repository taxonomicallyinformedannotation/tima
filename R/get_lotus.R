#' @title Get LOTUS
#'
#' @param url TODO
#' @param export TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom curl curl_download
#' @importFrom readr read_csv write_csv
#'
#' @examples TODO
get_lotus <-
  function(url = paths$urls$lotus,
           export = paths$data$source$libraries$lotus) {
    paths <- parse_yaml_paths()

    create_dir(export = export)

    readr::read_csv(file = curl::curl_download(url = url, destfile = tempfile())) |>
      readr::write_csv(file = export)
  }
