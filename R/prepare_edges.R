#' @title Prepare edges
#'
#' @param tool TODO
#' @param gnps_job_id TODO
#' @param input TODO
#' @param output TODO
#' @param name_source TODO
#' @param name_target TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom dplyr filter select
#' @importFrom readr read_delim write_delim
#' @importFrom stringr str_length
#'
#' @examples TODO
prepare_edges <- function(tool = params$tool,
                          gnps_job_id = params$gnps,
                          input = params$input,
                          output = params$output,
                          name_source = params$source_name,
                          name_target = params$target_name) {
  ## Check that tool is valid
  stopifnot("Your tool must be 'manual' or 'gnps" = tool %in% c("gnps", "manual"))

  ## Check that input is valid
  if (tool == "gnps") {
    stopifnot("Your GNPS job ID is invalid" = stringr::str_length(string = gnps_job_id) == 32)
  } else {
    stopifnot("Your input file does not exist" = file.exists(input))
  }

  ## Load edges table
  log_debug(x = "Loading edge table")
  if (tool == "gnps") {
    edges_table <- read_edges(id = gnps_job_id)
  } else {
    edges_table <- readr::read_delim(file = input)
  }

  ## Format edges table
  log_debug(x = "Formatting edge table")
  edges_table_treated <- edges_table |>
    dplyr::select(
      feature_source = name_source,
      feature_target = name_target
    ) |>
    dplyr::filter(feature_source != feature_target)

  ## Export edges table
  log_debug(x = "Exporting ...")
  export_params(step = "prepare_edges")
  export_output(x = edges_table_treated, file = output)
}
