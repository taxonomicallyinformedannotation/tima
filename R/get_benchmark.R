#' Title
#'
#' @param link TODO
#' @param export TODO
#'
#' @return TODO
#' @export
#'
#' @examples
get_benchmark <-
  function(link = paths$links$benchmarking_set,
           export = paths$data$source$benchmark$set) {
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

    readr::read_csv(file = curl::curl_download(url = link, destfile = tempfile())) |>
      readr::write_csv(file = export)
  }
