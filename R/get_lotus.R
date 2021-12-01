#' Title
#'
#' @param input TODO
#' @param output TODO
#'
#' @return TODO
#' @export
#'
#' @examples
get_lotus <-
  function(input = "https://osf.io/rheq5/download",
           output = "data/source/libraries/lotus.csv.gz") {
    readr::read_csv(file = curl::curl_download(input, tempfile())) |>
      readr::write_csv(file = output)
  }