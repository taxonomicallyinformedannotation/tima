#' @title Fake edges
#'
#' @description This function fakes edges in case none are given
#'
#' @param input Input file
#' @param output Output file
#' @param name_feature Name of the features column
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom readr read_delim write_delim
#'
#' @examples NULL
fake_features_edges <- function(input = params$files$features$raw,
                                output = params$files$networks$spectral$edges$processed,
                                name_feature = params$names$features) {
  stopifnot("Your input file does not exist" = file.exists(input))

  edges_table_treated <- readr::read_delim(
    file = input,
    col_select = c(
      feature_source = !!as.name(name_feature),
      feature_target = !!as.name(name_feature)
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
    step = "prepare_features_edges"
  )

  return(output)
}
