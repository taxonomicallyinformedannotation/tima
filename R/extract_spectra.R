#' Extract m/z values from peak data matrix
#' @keywords internal
.extract_mz_values <- function(peak_data) {
  if (is.matrix(peak_data) && ncol(peak_data) >= 1L) {
    peak_data[, 1L]
  } else {
    numeric(0)
  }
}

#' Extract intensity values from peak data matrix
#' @keywords internal
.extract_intensity_values <- function(peak_data) {
  if (is.matrix(peak_data) && ncol(peak_data) >= 2L) {
    peak_data[, 2L]
  } else {
    numeric(0)
  }
}

#' @title Extract spectra from Spectra object
#'
#' @description Extracts and harmonizes spectra data from a Spectra object into
#'     a flat data frame format. Handles column name inconsistencies, type
#'     conversions, and extracts peak lists (mz/intensity).
#'
#' @include logs_utils.R
#' @param object [Spectra] Spectra object from the Spectra package
#'
#' @return Data frame containing harmonized spectra metadata with mz and
#'     intensity list columns containing peak data
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Extract spectra from a Spectra object
#' library(Spectra)
#' spectra_df <- extract_spectra(spectra_object)
#'
#' # Access metadata and peaks
#' utils::head(spectra_df)
#' peaks <- spectra_df$mz[[1L]] # First spectrum peaks
#' }
extract_spectra <- function(object) {
  if (!inherits(object, "Spectra")) {
    cli::cli_abort(
      "input must be a Spectra object from the Spectra package",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Define Harmonization Mappings ----

  # Define column name mappings for harmonization
  # These handle inconsistencies across different data sources
  incoherent_colnames <- c(
    ms_level = "msLevel",
    precursor_intensity = "precursorIntensity",
    precursorMz = "PrecursorMZ"
  )

  # Define expected types for problematic columns
  incoherent_logical <- "predicted"
  incoherent_integer <- "spectrum_id"
  incoherent_numeric <- "PrecursorMZ"

  # Extract Spectra Metadata ----

  spectra <- as.data.frame(object@backend@spectraData)
  rownames(spectra) <- NULL

  # Harmonize Column Types & Names Before Wrapping ----
  s_names <- names(spectra)
  if (
    incoherent_logical %in%
      s_names &&
      !is.logical(spectra[[incoherent_logical]])
  ) {
    spectra[[incoherent_logical]] <- as.logical(spectra[[incoherent_logical]])
  }
  if (
    incoherent_integer %in%
      s_names &&
      !is.integer(spectra[[incoherent_integer]])
  ) {
    spectra[[incoherent_integer]] <- as.integer(spectra[[incoherent_integer]])
  }
  if (
    incoherent_numeric %in% s_names && !is.double(spectra[[incoherent_numeric]])
  ) {
    spectra[[incoherent_numeric]] <- as.numeric(spectra[[incoherent_numeric]])
  }

  columns_to_harmonize <- incoherent_colnames[
    unname(incoherent_colnames) %in% s_names
  ]
  if (length(columns_to_harmonize) > 0L) {
    target_names <- names(columns_to_harmonize)
    source_names <- unname(columns_to_harmonize)
    drop_existing <- intersect(target_names, setdiff(s_names, source_names))
    if (length(drop_existing) > 0L) {
      spectra[drop_existing] <- NULL
      s_names <- names(spectra)
    }
    m_pos <- match(source_names, s_names)
    names(spectra)[m_pos] <- target_names
  }

  # Extract Peak Data ----
  peaks_data <- object@backend@peaksData

  # Extract m/z values (first column of peak matrices)
  spectra[["mz"]] <- lapply(
    X = peaks_data,
    FUN = .extract_mz_values
  )

  # Extract intensity values (second column of peak matrices)
  spectra[["intensity"]] <- lapply(
    X = peaks_data,
    FUN = .extract_intensity_values
  )

  spectra <- tidytable::as_tidytable(spectra)

  log_debug("Extracted %d spectra", nrow(spectra))

  rm(
    incoherent_colnames,
    incoherent_logical,
    incoherent_integer,
    incoherent_numeric,
    columns_to_harmonize
  )
  spectra
}
