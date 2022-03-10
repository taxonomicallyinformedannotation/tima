#' Title
#'
#' @param input TODO
#' @param output TODO
#' @param name_source TODO
#' @param name_target TODO
#'
#' @return TODO
#' @export
#'
#' @examples
fake_edges <- function(input = params$input,
                       output = params$output,
                       name_source = params$source_name,
                       name_target = params$target_name) {
  stopifnot("Your input file does not exist" = file.exists(input))

  edges_table_treated <- readr::read_delim(file = input) |>
    dplyr::select(
      feature_source = name_source,
      feature_target = name_source
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
