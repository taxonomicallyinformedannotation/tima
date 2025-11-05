#' @title Import spectra
#'
#' @description This function imports mass spectra from various file formats
#'     (.mgf, .msp, .rds), harmonizes metadata field names, filters by MS level
#'     and polarity, optionally combines replicate spectra, and sanitizes peak data.
#'
#' @include read_mgf_opti.R
#' @include sanitize_spectra.R
#'
#' @param file Character string path to the spectrum file (.mgf, .msp, or .rds)
#' @param cutoff Numeric absolute minimal intensity threshold (default: 0)
#' @param dalton Numeric Dalton tolerance for peak matching (default: 0.01)
#' @param polarity Character string for polarity filtering: "pos", "neg", or NA
#'     to keep all (default: NA)
#' @param ppm Numeric PPM tolerance for peak matching (default: 10)
#' @param sanitize Logical flag indicating whether to sanitize spectra (default: TRUE)
#' @param combine Logical flag indicating whether to combine replicate spectra (default: TRUE)
#'
#' @return Spectra object containing the imported and processed spectra
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_file(
#'   url = get_default_paths()$urls$examples$spectra_mini,
#'   export = get_default_paths()$data$source$spectra
#' )
#' import_spectra(file = get_default_paths()$data$source$spectra)
#' import_spectra(
#'   file = get_default_paths()$data$source$spectra,
#'   sanitize = FALSE
#' )
#' }
import_spectra <- function(
  file,
  cutoff = 0,
  dalton = 0.01,
  polarity = NA,
  ppm = 10,
  sanitize = TRUE,
  combine = TRUE
) {
  # Validate inputs
  if (!file.exists(file)) {
    stop("Spectra file not found: ", file)
  }

  if (!is.na(polarity) && !polarity %in% c("pos", "neg")) {
    stop("Polarity must be 'pos', 'neg', or NA")
  }

  if (!is.numeric(cutoff) || cutoff < 0) {
    stop("Cutoff must be a non-negative number")
  }

  if (!is.numeric(dalton) || dalton < 0 || !is.numeric(ppm) || ppm < 0) {
    stop("Dalton and PPM tolerances must be positive numbers")
  }

  logger::log_info("Importing spectra from: {file}")
  logger::log_debug(
    "Parameters: cutoff={cutoff}, dalton={dalton}, ppm={ppm}, polarity={ifelse(is.na(polarity), 'all', polarity)}"
  )

  # Extract file extension (handle MassBank naming convention)
  file_ext <- file |>
    stringi::stri_replace_all_regex(
      pattern = ".*\\.",
      replacement = "",
      vectorize_all = FALSE
    ) |>
    stringi::stri_replace_all_regex(
      pattern = "_.*",
      replacement = "",
      vectorize_all = FALSE
    )

  logger::log_trace("Detected file format: {file_ext}")

  # Import spectra based on file format
  spectra <- tryCatch(
    {
      switch(
        EXPR = file_ext,
        "mgf" = {
          logger::log_debug("Reading MGF file...")
          read_mgf_opti(f = file) |>
            Spectra::Spectra()
        },
        "msp" = {
          logger::log_debug("Reading MSP file...")
          MsBackendMsp::readMsp(f = file) |>
            Spectra::Spectra()
        },
        "rds" = {
          logger::log_debug("Reading RDS file...")
          readRDS(file = file)
        },
        stop(
          "Unsupported file format: '",
          file_ext,
          "'. Supported formats: mgf, msp, rds"
        )
      )
    },
    error = function(e) {
      logger::log_error("Failed to import spectra: {conditionMessage(e)}")
      stop("Failed to import spectra: ", conditionMessage(e))
    }
  )

  n_initial <- length(spectra)
  logger::log_info("Loaded {n_initial} spectra from file")

  # Validate precursor charges
  if (0L %in% spectra@backend@spectraData$precursorCharge) {
    logger::log_warn(
      "Found {sum(spectra@backend@spectraData$precursorCharge == 0)} spectra with precursorCharge = 0 ",
      "(unknown polarity - should be avoided in practice)"
    )
  }

  # Harmonize metadata field names across different data sources
  if ("MSLEVEL" %in% colnames(spectra@backend@spectraData)) {
    spectra$msLevel <- as.integer(spectra$MSLEVEL)
  }

  if ("MS_LEVEL" %in% colnames(spectra@backend@spectraData)) {
    spectra$msLevel <- as.integer(spectra$MS_LEVEL)
  }

  if ("PRECURSOR_MZ" %in% colnames(spectra@backend@spectraData)) {
    spectra$precursorMz <- as.numeric(spectra$PRECURSOR_MZ)
  }

  if ("spectrum_id" %in% colnames(spectra@backend@spectraData)) {
    spectra$spectrum_id <- as.character(spectra$spectrum_id)
  }

  # Filter to MS2 spectra only
  if ("msLevel" %in% colnames(spectra@backend@spectraData)) {
    n_before <- length(spectra)
    spectra <- Spectra::filterMsLevel(spectra, 2L)
    n_after <- length(spectra)
    logger::log_debug(
      "Filtered to MS2 spectra: {n_before} -> {n_after} spectra"
    )
  }

  # Handle MassBank-specific MS level field
  if ("Spectrum_type" %in% colnames(spectra@backend@spectraData)) {
    n_before <- length(spectra)
    spectra <- spectra[spectra@backend@spectraData$Spectrum_type == "MS2"]
    n_after <- length(spectra)
    logger::log_debug(
      "Filtered to MS2 spectra (MassBank): {n_before} -> {n_after} spectra"
    )
  }

  # Filter by polarity if specified
  if (!is.na(polarity)) {
    n_before <- length(spectra)
    charge_values <- if (polarity == "pos") {
      c(0L, 1L, 2L, 3L) # Include 0 for broken MGFs
    } else {
      c(0L, -1L, -2L, -3L)
    }

    spectra <- Spectra::filterPrecursorCharge(spectra, z = charge_values)
    n_after <- length(spectra)
    logger::log_debug(
      "Filtered to {polarity} polarity: {n_before} -> {n_after} spectra"
    )
  }

  # Combine replicate spectra if requested
  if (combine) {
    n_before <- length(spectra)
    if ("FEATURE_ID" %in% colnames(spectra@backend@spectraData)) {
      logger::log_debug("Combining replicate spectra by FEATURE_ID")
      spectra <- spectra |>
        Spectra::combineSpectra(
          f = spectra$FEATURE_ID,
          tolerance = dalton,
          ppm = ppm
        ) |>
        Spectra::combinePeaks(tolerance = dalton, ppm = ppm)
      n_after <- length(spectra)
      logger::log_debug("Combined replicates: {n_before} -> {n_after} spectra")
    } else if ("SLAW_ID" %in% colnames(spectra@backend@spectraData)) {
      logger::log_debug("Combining replicate spectra by SLAW_ID")
      spectra <- spectra |>
        Spectra::combineSpectra(
          f = spectra$SLAW_ID,
          tolerance = dalton,
          ppm = ppm
        ) |>
        Spectra::combinePeaks(tolerance = dalton, ppm = ppm)
      n_after <- length(spectra)
      logger::log_debug("Combined replicates: {n_before} -> {n_after} spectra")
    } else {
      logger::log_trace(
        "No replicate grouping field found, skipping combination"
      )
    }
  }

  # Sanitize spectra if requested
  if (sanitize) {
    n_before <- length(spectra)
    logger::log_debug("Sanitizing spectra (cutoff={cutoff})")
    spectra <- sanitize_spectra(
      spectra = spectra,
      cutoff = cutoff,
      dalton = dalton,
      ppm = ppm
    )
    n_after <- length(spectra)
    logger::log_debug("Sanitization complete: {n_before} -> {n_after} spectra")
  }

  logger::log_info(
    "Import complete: {length(spectra)} spectra ready for analysis"
  )

  return(spectra)
}
