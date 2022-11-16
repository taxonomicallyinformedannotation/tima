#' @title Get example feature table
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
get_example_feature_table <-
  function(url = paths$urls$example_feature_table,
           export = paths$
             data$
             interim$
             annotations$
             example_feature_table) {
    paths <- parse_yaml_paths()

    create_dir(export = export)

    readr::read_tsv(
      file = url,
      col_select = c(
        feature_id = `cluster index`,
        rt = RTMean,
        mz = `precursor mass`
      )
    ) |>
      readr::write_tsv(file = export)
  }
