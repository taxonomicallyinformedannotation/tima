#' Title
#'
#' @param url TODO
#' @param export TODO
#'
#' @return TODO
#' @export
#'
#' @examples
get_example_feature_table <-
  function(url = paths$urls$example_feature_table,
           export = paths$data$interim$annotations$example_feature_table) {
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
      dplyr::select(
        feature_id = `cluster index`,
        rt = RTMean,
        mz = `precursor mass`
      ) |>
      readr::write_tsv(file = export)
  }
