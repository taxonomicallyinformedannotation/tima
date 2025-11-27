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
    stop("example must be a non-empty character vector")
  }

  invalid_examples <- setdiff(example, valid_examples)
  if (length(invalid_examples) > 0L) {
    stop(
      "Invalid example file(s): ",
      paste(invalid_examples, collapse = ", "),
      "\nValid options: ",
      paste(valid_examples, collapse = ", ")
    )
  }

  if (!is.logical(in_cache) || length(in_cache) != 1L) {
    stop("in_cache must be a single logical value (TRUE/FALSE)")
  }

  logger::log_debug(
    "Downloading {length(example)} example file(s): {paste(example, collapse = ', ')}"
  )

  # Navigate to cache if requested
  if (in_cache) {
    go_to_cache()
  }

  # Download each requested example file
  if ("features" %in% example) {
    logger::log_debug("Downloading features example")
    get_file(
      url = get_default_paths()$urls$examples$features,
      export = get_default_paths()$data$source$features
    )
  }
  # if ("hmdb_is" %in% example) {
  #   logger::log_trace("HMDB in silico")
  #   get_file(
  #     url = get_default_paths()$urls$hmdb$spectra$predicted,
  #     export = get_default_paths()$data$source$libraries$spectra$is$hmdb
  #   )
  # }
  if ("metadata" %in% example) {
    logger::log_debug("Downloading metadata example")
    get_file(
      url = get_default_paths()$urls$examples$metadata,
      export = get_default_paths()$data$source$metadata
    )
  }

  if ("sirius" %in% example) {
    logger::log_debug("Downloading SIRIUS examples")
    get_example_sirius()
  }

  if ("spectra" %in% example) {
    logger::log_debug("Downloading spectra example")
    get_file(
      url = get_default_paths()$urls$examples$spectra,
      # url = get_default_paths()$urls$examples$spectra_mini,
      export = get_default_paths()$data$source$spectra
    )
  }

  if ("spectral_lib_with_rt" %in% example) {
    logger::log_debug("Downloading spectral library with retention times")
    get_file(
      url = get_default_paths()$urls$examples$spectral_lib_mini$with_rt,
      export = get_default_paths()$data$source$libraries$spectra$exp$with_rt
    )
  }

  logger::log_info("Downloaded {length(example)} example file(s)")
  invisible(NULL)
}
