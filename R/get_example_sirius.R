#' @title Get example sirius
#'
#' @description This function gets example SIRIUS annotations
#'
#' @include get_file.R
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
  function(url = paths$urls$examples$sirius,
           export = paths$data$interim$annotations$example_sirius) {
    get_file(
      url = url,
      export = export
    )
    message("Unzipping")
    utils::unzip(
      zipfile = export,
      exdir = dirname(export)
    )
  }
