#' @title Fake edges
#'
#' @param input TODO
#' @param output TODO
#' @param name_feature TODO
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom readr read_delim write_delim
#'
#' @examples TODO
fake_edges <- function(input = params$input,
                       output = params$output,
                       name_feature = params$feature) {
  stopifnot("Your input file does not exist" = file.exists(input))

  edges_table_treated <- readr::read_delim(
    file = input,
    col_select = c(
      feature_source = name_feature,
      feature_target = name_feature
    )
  )

  log_debug("Exporting ...")
  create_dir(paths$data$interim$edges$path)
  create_dir(paths$data$interim$config$path)
  create_dir(output)

  log_debug(
    "... path to export is",
    output
  )
  readr::write_delim(
    x = edges_table_treated,
    file = output,
    delim = "\t"
  )

  export_params(
    parameters = params,
    directory = paths$data$interim$config$path,
    step = "prepare_edges"
  )
}
