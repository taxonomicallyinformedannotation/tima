#' Title
#'
#' @param tool TODO
#' @param gnps_job TODO
#' @param input TODO
#' @param output TODO
#' @param name_source TODO
#' @param name_target TODO
#'
#' @return TODO
#' @export
#'
#' @examples
prepare_edges <- function(tool = params$tool,
                          gnps_job = params$gnps,
                          input = params$input,
                          output = params$output,
                          name_source = params$source_name,
                          name_target = params$target_name) {
  stopifnot(
    "Your --tool parameter (in command line arguments or in 'treat_params.yaml' must be 'manual' or 'gnps" = tool %in% c("gnps", "manual")
  )

  log_debug(x = "Loading edge table")
  if (tool == "gnps") {
    edges_table <- read_edges(gnps_job)
  }

  if (tool == "manual") {
    edges_table <-
      readr::read_delim(file = input)
  }

  log_debug(x = "Formatting edge table")
  edges_table_treated <- edges_table |>
    dplyr::select(
      feature_source = name_source,
      feature_target = name_target
    ) |>
    dplyr::filter(feature_source != feature_target)

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
