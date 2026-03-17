#' @title Get example sirius
#'
#' @description This function downloads example SIRIUS annotation files for
#'     testing and demonstration purposes. Downloads both SIRIUS v5 and v6
#'     format files.
#'
#' @include get_default_paths.R
#' @include get_file.R
#' @include logs_utils.R
#'
#' @param url [list] List containing URLs for SIRIUS examples (must have $v5 and $v6 elements)
#' @param export [list] List containing export paths for SIRIUS examples (must have $v5 and $v6 elements)
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
    cli::cli_abort(
      "{.arg url} must be a list with {.field v5} and {.field v6} elements",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (!is.list(export) || is.null(export$v5) || is.null(export$v6)) {
    cli::cli_abort(
      "{.arg export} must be a list with {.field v5} and {.field v6} elements",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  log_debug("Downloading SIRIUS example files (v5 and v6)")

  # Download SIRIUS v5 example
  get_file(url = url$v5, export = export$v5)

  # Download SIRIUS v6 example (with path adjustment)
  v6_export <- export$v6 |>
    gsub(pattern = "_6", replacement = "", fixed = TRUE)
  get_file(url = url$v6, export = v6_export)

  log_info("Downloaded SIRIUS examples")
  invisible(NULL)
}
