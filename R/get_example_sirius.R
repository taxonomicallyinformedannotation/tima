#' @title Get example sirius
#'
#' @description This function gets example SIRIUS annotations
#'
#' @include get_default_paths.R
#' @include get_file.R
#'
#' @param url URL where the example is accessible
#' @param export Path where to save the example
#'
#' @return NULL
#'
#' @examples NULL
get_example_sirius <-
  function(
    url = get_default_paths()$urls$examples$sirius,
    export = get_default_paths()$data$interim$annotations$example_sirius
  ) {
    get_file(url = url$v5, export = export$v5)
    get_file(
      url = url$v6,
      export = export$v6 |>
        gsub(pattern = "_6", replacement = "")
    )
  }
