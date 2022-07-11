#' Title
#'
#' @param url TODO
#' @param export TODO
#'
#' @return TODO
#' @export
#'
#' @importFrom readr read_tsv write_tsv
#'
#' @examples
get_example_isdb <-
  function(url = paths$urls$example_isdb,
           export = paths$data$interim$annotations$example_isdb) {
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

    readr::read_tsv(file = url) |>
      readr::write_tsv(file = export)
  }
