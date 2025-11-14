#' @title Extract spectra from a Spectra object
#'
#' @description This function extracts and harmonizes spectra data from a
#'     Spectra object into a flat data frame format. It handles column name
#'     inconsistencies, type conversions, and extracts peak lists (mz/intensity).
#'
#' @param object Spectra object from the Spectra package
#'
#' @return Data frame containing harmonized spectra metadata with additional
#'     mz and intensity list columns containing peak data
#'
#' @examples NULL
extract_spectra <- function(object) {
  # ============================================================================
  # Input Validation
  # ============================================================================

  # Validate input
  if (!inherits(object, "Spectra")) {
    stop("Input must be a Spectra object from the Spectra package")
  }

  # ============================================================================
  # Define Harmonization Mappings
  # ============================================================================

  # Define column name mappings for harmonization
  # These handle inconsistencies across different data sources
  incoherent_colnames <- c(
    ms_level = "msLevel",
    precursor_intensity = "precursorIntensity",
    precursorMz = "PrecursorMZ"
  )

  # Define expected types for problematic columns
  incoherent_logical <- c("predicted")
  incoherent_integer <- c("spectrum_id")
  incoherent_numeric <- c("PrecursorMZ")

  # ============================================================================
  # Extract Spectra Metadata
  # ============================================================================

  # Extract spectra metadata
  logger::log_trace("Extracting spectra metadata")
  spectra <- object@backend@spectraData |>
    data.frame() |>
    tidytable::as_tidytable()

  # ============================================================================
  # Extract Peak Data
  # ============================================================================

  # Extract peak data (mz and intensity) efficiently
  logger::log_trace("Extracting peak data (mz and intensity)")
  spectra$mz <- lapply(
    X = object@backend@peaksData,
    FUN = function(peakData) {
      if (is.matrix(peakData) && ncol(peakData) >= 1L) {
        peakData[, 1L]
      } else {
        numeric(0)
      }
    }
  )

  spectra$intensity <- lapply(
    X = object@backend@peaksData,
    FUN = function(peakData) {
      if (is.matrix(peakData) && ncol(peakData) >= 2L) {
        peakData[, 2L]
      } else {
        numeric(0)
      }
    }
  )

  ## Synonyms issue
  # spectra <- spectra |>
  # tidytable::group_by(c(-tidyselect::any_of("synonym"))) |>
  # tidytable::reframe(tidytable::across(
  # .cols = tidyselect::where(is.list),
  # .fns = as.character
  # )) |>
  # tidytable::ungroup()

  # ============================================================================
  # Harmonize Column Types
  # ============================================================================

  # Harmonize column types
  logger::log_trace("Harmonizing column types")
  spectra <- spectra |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::any_of(incoherent_logical),
      .fns = as.logical
    )) |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::any_of(incoherent_integer),
      .fns = as.integer
    )) |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::any_of(incoherent_numeric),
      .fns = as.numeric
    ))

  # ============================================================================
  # Harmonize Column Names
  # ============================================================================

  # Harmonize column names: remove old names and add standardized names
  # Only process columns that actually exist in the data
  columns_to_harmonize <- incoherent_colnames[
    unname(incoherent_colnames) %in% colnames(spectra)
  ]

  if (length(columns_to_harmonize) > 0L) {
    logger::log_trace(
      "Harmonizing {length(columns_to_harmonize)} column names"
    )
    spectra <- spectra |>
      tidytable::select(-tidyselect::any_of(names(columns_to_harmonize))) |>
      tidytable::rename(tidyselect::any_of(columns_to_harmonize))
  }

  logger::log_debug("Extracted {nrow(spectra)} spectra")

  return(spectra)
}
