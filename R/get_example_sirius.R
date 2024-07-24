import::from(utils, unzip, .into = environment())

#' @title Get example sirius
#'
#' @description This function gets example SIRIUS annotations
#'
#' @importFrom utils unzip
#'
#' @include parse_yaml_paths.R get_file.R
#'
#' @param url URL where the example is accessible
#' @param export Path where to save the example
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
get_example_sirius <-
  function(url = parse_yaml_paths()$urls$examples$sirius,
           export = parse_yaml_paths()$data$interim$annotations$example_sirius) {
    get_file(url = url$v5, export = export$v5)
    get_file(url = url$v6, export = export$v6)
    unzip(
      zipfile = export$v6,
      exdir = dirname(export$v6),
      overwrite = TRUE
    )
  }
