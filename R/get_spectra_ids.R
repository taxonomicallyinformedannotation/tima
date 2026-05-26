#' @title Get Spectra IDs
#'
#' @description Extracts spectrum identifiers from a [Spectra::Spectra()] object.
#'   Different data sources use different field names for spectrum IDs; this
#'   function checks multiple common field names in priority order.
#'
#' @details Checks for ID fields in the following order:
#' 1. `SLAW_ID` — SLAW-format export.
#' 2. `FEATURE_ID` — TIMA mzTab-M proxy / embedded MGF export, MZmine, etc.
#' 3. `acquisitionNum` — mzML / open-access raw-file formats.
#' 4. `spectrum_id` — generic Spectra ID field.
#' 5. `TITLE` field, parsed for the pattern `id:<digits>` (e.g.
#'    `"id:42, rt:168.84, mz:293.1762, energy:nan, ..."` from newer masster
#'    variants).  Patterns like `uid:`, `scan_id:`, and `_id:` are explicitly
#'    excluded by the negative look-behind.
#'
#' @note When the TITLE fallback is triggered a warning is logged.  The
#'   preferred approach is for the MGF writer to emit a dedicated `FEATURE_ID=`
#'   field (which [.extract_embedded_mgf()] and [.write_proxy_mgf()] both do).
#'
#' @param spectra [Spectra::Spectra()] object.
#'
#' @return Character vector of spectrum IDs (one per spectrum), or `NULL` if no
#'   recognised ID field is found.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Extract IDs from a Spectra object
#' spec_ids <- get_spectra_ids(my_spectra)
#' }
get_spectra_ids <- function(spectra) {
  # Validate input
  if (!inherits(spectra, "Spectra")) {
    cli::cli_abort(
      "input must be a Spectra object from the Spectra package",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Safe accessor: works across different Spectra backend generations
  spec_data <- spectra@backend@spectraData

  # Priority order of dedicated ID field names
  possible_ids <- c("SLAW_ID", "FEATURE_ID", "acquisitionNum", "spectrum_id")

  # Find the first valid ID field
  valid_field <- purrr::detect(
    possible_ids,
    ~ !is.null(spec_data[[.x]]) && length(spec_data[[.x]]) > 0L
  )

  if (!is.null(valid_field)) {
    return(spec_data[[valid_field]])
  }

  # Last-resort: parse "id:<N>" from TITLE field.
  # Handles newer masster variants that embed the feature ID in TITLE as
  # "id:42, rt:168.84, mz:293.1762, energy:nan, ...".
  # The negative look-behind excludes "uid:", "scan_id:", "_id:", etc.
  if (!is.null(spec_data[["TITLE"]])) {
    titles <- spec_data[["TITLE"]]
    parsed <- stringi::stri_match_first_regex(
      titles,
      "(?<![_a-zA-Z])id:(\\d+)"
    )[, 2L]
    if (!all(is.na(parsed))) {
      log_warn(
        paste0(
          "No dedicated ID field found; extracted feature IDs from TITLE. ",
          "Consider adding FEATURE_ID= to the MGF writer."
        )
      )
      return(as.character(parsed))
    }
  }

  log_warn(
    "No valid spectrum ID field found. Checked fields: %s, TITLE",
    paste(possible_ids, collapse = ", ")
  )
  NULL
}
