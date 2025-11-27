#' @title Export spectra to RDS file
#'
#' @description Exports a Spectra object to an RDS file format for efficient
#'     storage and later retrieval. Only exports spectra with valid compound IDs.
#'
#' @include create_dir.R
#' @include validations_utils.R
#'
#' @param file Path where spectra will be exported as RDS file
#' @param spectra Spectra object containing spectral data to export
#'
#' @return NULL (invisibly). Saves spectra to file as a side effect.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Export spectra to RDS file
#' library(Spectra)
#' export_spectra_rds(
#'   file = "data/processed/spectra.rds",
#'   spectra = my_spectra_object
#' )
#' }
export_spectra_rds <- function(file, spectra) {
  # Input Validation ----
  validate_character(file, param_name = "file", allow_empty = FALSE)

  if (!inherits(spectra, "Spectra")) {
    stop("spectra must be a Spectra object", call. = FALSE)
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

  # logger::log_trace("Successfully exported spectra")
  invisible(NULL)
}
