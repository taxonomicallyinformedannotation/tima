#' @title Get benchmark
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
get_benchmark <-
  function(url = paths$urls$benchmarking_set,
           export = paths$data$source$benchmark$set) {
    paths <- parse_yaml_paths()

    create_dir(export = export)

    readr::read_csv(file = curl::curl_download(url = url, destfile = tempfile())) |>
      readr::write_csv(file = export)
  }
