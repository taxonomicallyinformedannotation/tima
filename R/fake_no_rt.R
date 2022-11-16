#' @title Fake no retention time
#'
#' @param input TODO
#' @param output TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom readr read_delim write_delim
#'
#' @examples TODO
fake_no_rt <-
  function(input = paths$
             data$
             interim$
             annotations$
             example_feature_table,
           output = paths$
             data$
             interim$
             annotations$
             example_feature_table_no_rt) {
    paths <- parse_yaml_paths()
    stopifnot("Your input file does not exist" = file.exists(input))

    feature_table <- readr::read_delim(
      file = input,
      col_select = c(-rt)
    )

    log_debug(x = "Exporting ...")
    export_output(x = feature_table)
  }
