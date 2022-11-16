#' @title Get last version from Zenodo
#'
#' @param url TODO
#' @param pattern TODO
#' @param delim TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom curl curl_download
#' @importFrom purrr pluck
#' @importFrom readr read_delim
#' @importFrom rvest session
#'
#' @examples TODO
get_last_version_from_zenodo <-
  function(url, pattern, delim = ",") {
    file <- rvest::session(url = url) |>
      follow_next(text = pattern) |>
      purrr::pluck("url") |>
      curl::curl_download(destfile = tempfile()) |>
      readr::read_delim(delim = delim)

    return(file)
  }
