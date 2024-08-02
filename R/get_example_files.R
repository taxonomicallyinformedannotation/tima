#' @title Get example files
#'
#' @description This function downloads example files
#'
#' @include get_example_sirius.R
#' @include get_file.R
#' @include go_to_cache.R
#' @include parse_yaml_paths.R
#'
#' @return Example files.
#'
#' @export
#'
#' @examples NULL
get_example_files <- function() {
  go_to_cache()
  message("Features")
  get_file(
    url = tima::parse_yaml_paths()$urls$examples$features,
    export = tima::parse_yaml_paths()$data$source$features
  )
  message("Metadata")
  get_file(
    url = tima::parse_yaml_paths()$urls$examples$metadata,
    export = tima::parse_yaml_paths()$data$source$metadata
  )
  message("Sirius")
  get_example_sirius()
  message("Spectra")
  get_file(
    url = tima::parse_yaml_paths()$urls$examples$spectra,
    export = tima::parse_yaml_paths()$data$source$spectra
  )
}
