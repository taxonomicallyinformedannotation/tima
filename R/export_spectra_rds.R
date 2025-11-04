#' @title Export spectra RDS
#'
#' @description This function exports a Spectra object to an RDS file format
#'     for efficient storage and later retrieval. Only exports spectra with
#'     valid compound IDs.
#'
#' @include create_dir.R
#'
#' @param file Character string path where spectra will be exported as RDS file
#' @param spectra Spectra object containing spectral data to export
#'
#' @return NULL (invisibly). Saves spectra to file as a side effect.
#'
#' @examples NULL
export_spectra_rds <- function(file, spectra) {
  # Validate inputs
  if (missing(file) || !is.character(file) || length(file) != 1L) {
    stop("file must be a single character string")
  }

  if (missing(spectra) || !inherits(spectra, "Spectra")) {
    stop("spectra must be a Spectra object")
  }

  # Filter to spectra with valid compound IDs
  valid_spectra <- spectra[!is.na(spectra$compound_id)]

  if (length(valid_spectra) == 0L) {
    logger::log_warn("No spectra with valid compound IDs to export")
    return(invisible(NULL))
  }

  logger::log_debug("Exporting ", length(valid_spectra), " spectra to: ", file)

  # Create output directory if needed
  create_dir(export = file)

  # Save spectra as RDS
  saveRDS(valid_spectra, file = file)

  logger::log_trace("Successfully exported spectra")
  invisible(NULL)
}
