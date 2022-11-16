#' @title Get example ISDB
#'
#' @param url TODO
#' @param export TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom readr read_tsv write_tsv
#'
#' @examples TODO
get_example_isdb <-
  function(url = paths$urls$example_isdb,
           export = paths$data$interim$annotations$example_isdb) {
    paths <- parse_yaml_paths()

    create_dir(export = export)

    readr::read_tsv(file = url) |>
      readr::write_tsv(file = export)
  }
