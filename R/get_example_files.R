#' @title Get example files
#'
#' @description This function downloads example files
#'
#' @include get_example_sirius.R
#' @include get_file.R
#' @include go_to_cache.R
#' @include parse_yaml_paths.R
#'
#' @param example The example(s) you want to download
#'
#' @return Example files.
#'
#' @export
#'
#' @examples NULL
get_example_files <- function(example = c("features", "metadata", "sirius", "spectra")) {
  stopifnot(
    "Example files available are `features`, `hmdb_is`, `metadata`, `sirius`, `spectra` and `spectral_lib_with_rt`." =
      example %in% c(
        "features",
        "hmdb_is",
        "metadata",
        "sirius",
        "spectra",
        "spectral_lib_with_rt"
      )
  )
  go_to_cache()
  if ("features" %in% example) {
    message("Features")
    get_file(
      url = tima::parse_yaml_paths()$urls$examples$features,
      export = tima::parse_yaml_paths()$data$source$features
    )
  }
  if ("hmdb_is" %in% example) {
    message("HMDB in silico")
    get_file(
      url = tima::parse_yaml_paths()$urls$hmdb$spectra$predicted,
      export = tima::parse_yaml_paths()$data$source$libraries$spectra$is$hmdb
    )
  }
  if ("metadata" %in% example) {
    message("Metadata")
    get_file(
      url = tima::parse_yaml_paths()$urls$examples$metadata,
      export = tima::parse_yaml_paths()$data$source$metadata
    )
  }
  if ("sirius" %in% example) {
    message("Sirius")
    get_example_sirius()
  }
  if ("spectra" %in% example) {
    message("Spectra")
    get_file(
      url = tima::parse_yaml_paths()$urls$examples$spectra,
      export = tima::parse_yaml_paths()$data$source$spectra
    )
  }
  if ("spectral_lib_with_rt" %in% example) {
    message("Spectral library with retention times")
    get_file(
      url = tima::parse_yaml_paths()$urls$examples$spectral_lib_mini$with_rt,
      export = tima::parse_yaml_paths()$data$source$libraries$spectra$exp$with_rt
    )
  }
}
