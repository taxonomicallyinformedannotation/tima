#' @title Get example files
#'
#' @description This function downloads example data files for testing and
#'     demonstration purposes. Supports downloading features, metadata, SIRIUS
#'     annotations, mass spectra, and spectral libraries with retention times.
#'
#' @include get_default_paths.R
#' @include get_example_sirius.R
#' @include get_file.R
#' @include go_to_cache.R
#' @include logs_utils.R
#'
#' @param example Character vector specifying which example files to download.
#'     Valid options: "features", "metadata", "sirius", "spectra", "spectral_lib_with_rt"
#' @param in_cache Logical whether to store files in the cache directory (default: TRUE)
#'
#' @return NULL (invisibly). Downloads files as a side effect.
#'
#' @family data-retrieval
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Download features and metadata examples
#' get_example_files(example = c("features", "metadata"))
#'
#' # Download all example files to cache
#' get_example_files(
#'   example = c("features", "metadata", "sirius", "spectra"),
#'   in_cache = TRUE
#' )
#' }
get_example_files <- function(
  example = c("features", "metadata", "sirius", "spectra"),
  in_cache = TRUE
) {
  # Define valid example types
  valid_examples <- c(
    "features",
    # "hmdb_is",
    "metadata",
    "sirius",
    "spectra",
    "spectral_lib_with_rt"
  )

  # Validate inputs
  if (!is.character(example) || length(example) == 0L) {
    cli::cli_abort(
      "{.arg example} must be a non-empty character vector",
      class = c("tima_validation_error", "tima_error")
    )
  }

  invalid_examples <- setdiff(example, valid_examples)
  if (length(invalid_examples) > 0L) {
    cli::cli_abort(
      c(
        "invalid {.arg example} value{?s}: {.val {invalid_examples}}",
        "i" = "Valid options: {.val {valid_examples}}"
      ),
      class = c("tima_validation_error", "tima_error")
    )
  }

  if (!is.logical(in_cache) || length(in_cache) != 1L) {
    cli::cli_abort(
      "{.arg in_cache} must be a single logical value",
      class = c("tima_validation_error", "tima_error")
    )
  }

  log_debug(
    "Downloading %d example file(s): %s",
    length(example),
    paste(example, collapse = ", ")
  )

  # Navigate to cache if requested
  if (in_cache) {
    go_to_cache()
  }

  # Download each requested example file
  if ("features" %in% example) {
    log_debug("Downloading features example")
    get_file(
      url = get_default_paths()$urls$examples$features,
      export = get_default_paths()$data$source$features
    )
  }
  # if ("hmdb_is" %in% example) {
  #   log_trace("HMDB in silico")
  #   get_file(
  #     url = get_default_paths()$urls$hmdb$spectra$predicted,
  #     export = get_default_paths()$data$source$libraries$spectra$is$hmdb
  #   )
  # }
  if ("metadata" %in% example) {
    log_debug("Downloading metadata example")
    get_file(
      url = get_default_paths()$urls$examples$metadata,
      export = get_default_paths()$data$source$metadata
    )
  }

  if ("sirius" %in% example) {
    log_debug("Downloading SIRIUS examples")
    get_example_sirius()
  }

  if ("spectra" %in% example) {
    log_debug("Downloading spectra example")
    get_file(
      url = get_default_paths()$urls$examples$spectra,
      # url = get_default_paths()$urls$examples$spectra_mini,
      export = get_default_paths()$data$source$spectra
    )
  }

  if ("spectral_lib_with_rt" %in% example) {
    log_debug("Downloading spectral library with retention times")
    get_file(
      url = get_default_paths()$urls$examples$spectral_lib_mini$with_rt,
      export = get_default_paths()$data$source$libraries$spectra$exp$with_rt
    )
  }

  log_info("Downloaded %d example file(s)", length(example))
  invisible(NULL)
}
