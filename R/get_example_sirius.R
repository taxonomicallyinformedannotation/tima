#' @title Get example sirius
#'
#' @description This function downloads example SIRIUS annotation files for
#'     testing and demonstration purposes. Downloads both SIRIUS v5 and v6
#'     format files.
#'
#' @include get_default_paths.R
#' @include get_file.R
#'
#' @param url List containing URLs for SIRIUS examples (must have $v5 and $v6 elements)
#' @param export List containing export paths for SIRIUS examples (must have $v5 and $v6 elements)
#'
#' @return NULL (invisibly). Downloads files as a side effect.
#'
#' @examples
#' \dontrun{
#' get_example_sirius()
#' }
get_example_sirius <- function(
  url = get_default_paths()$urls$examples$sirius,
  export = get_default_paths()$data$interim$annotations$example_sirius
) {
  # Validate inputs
  if (!is.list(url) || is.null(url$v5) || is.null(url$v6)) {
    stop("url must be a list with $v5 and $v6 elements")
  }

  if (!is.list(export) || is.null(export$v5) || is.null(export$v6)) {
    stop("export must be a list with $v5 and $v6 elements")
  }

  logger::log_info("Downloading example SIRIUS annotation files")

  # Download SIRIUS v5 example
  logger::log_debug("Downloading SIRIUS v5 example")
  get_file(url = url$v5, export = export$v5)

  # Download SIRIUS v6 example (with path adjustment)
  logger::log_debug("Downloading SIRIUS v6 example")
  v6_export <- export$v6 |>
    gsub(pattern = "_6", replacement = "", fixed = TRUE)
  get_file(url = url$v6, export = v6_export)

  logger::log_info("Successfully downloaded SIRIUS example files")
  invisible(NULL)
}
