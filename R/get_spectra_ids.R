#' @title Get Spectra IDs
#'
#' @description This function extracts spectrum identifiers from a Spectra object.
#'     Since different data sources use different field names for spectrum IDs,
#'     this function checks multiple common field names in priority order.
#'
#' @details Checks for ID fields in this order: SLAW_ID, FEATURE_ID,
#'     acquisitionNum, spectrum_id. Returns the first valid field found.
#'
#' @param spectra Spectra object from the Spectra package
#'
#' @return Character vector of spectrum IDs, or NULL if no valid ID field found
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Extract IDs from Spectra object
#' spec_ids <- get_spectra_ids(my_spectra)
#' }
get_spectra_ids <- function(spectra) {
  # Validate input
  if (!inherits(spectra, "Spectra")) {
    stop("Input must be a Spectra object from the Spectra package")
  }

  # Priority order of ID field names to check
  # Note: Different data sources use different naming conventions
  possible_ids <- c("SLAW_ID", "FEATURE_ID", "acquisitionNum", "spectrum_id")

  # Find the first valid ID field
  valid_field <- purrr::detect(
    possible_ids,
    ~ !is.null(spectra@backend@spectraData[[.x]]) &&
      length(spectra@backend@spectraData[[.x]]) > 0L
  )

  if (!is.null(valid_field)) {
    # log_trace("Found spectrum IDs in field: ", valid_field)
    return(spectra@backend@spectraData[[valid_field]])
  } else {
    log_warn(
      "No valid spectrum ID field found. Checked fields: %s",
      paste(possible_ids, collapse = ", ")
    )
    return(NULL)
  }
}
