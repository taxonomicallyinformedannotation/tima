if (!require(readr)) {
  install.packages("readr")
  require(package = "readr", quietly = TRUE)
}

#' Title
#'
#' @param input TODO
#' @param output TODO
#' @param name_feature TODO
#'
#' @return TODO
#' @export
#'
#' @importFrom readr read_delim write_delim
#'
#' @examples
fake_edges <- function(input = params$input,
                       output = params$output,
                       name_feature = params$feature) {
  stopifnot("Your input file does not exist" = file.exists(input))

  edges_table_treated <- readr::read_delim(
    file = input,
    col_select =
      c(
        feature_source = name_feature,
        feature_target = name_feature
      )
  )

  log_debug(x = "Exporting ...")
  ifelse(
    test = !dir.exists(paths$data$path),
    yes = dir.create(paths$data$path),
    no = paste(paths$data$path, "exists")
  )
  ifelse(
    test = !dir.exists(paths$data$interim$path),
    yes = dir.create(paths$data$interim$path),
    no = paste(paths$data$interim$path, "exists")
  )
  ifelse(
    test = !dir.exists(paths$data$interim$edges$path),
    yes = dir.create(paths$data$interim$edges$path),
    no = paste(paths$data$interim$edges$path, "exists")
  )
  ifelse(
    test = !dir.exists(paths$data$interim$config$path),
    yes = dir.create(paths$data$interim$config$path),
    no = paste(paths$data$interim$config$path, "exists")
  )
  ifelse(
    test = !dir.exists(dirname(output)),
    yes = dir.create(dirname(output)),
    no = paste(dirname(output), "exists")
  )

  log_debug(
    x = "... path to export is",
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
