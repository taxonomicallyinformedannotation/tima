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
  # Input Validation ----

  # Validate numeric parameters first (cheapest checks)
  if (!is.numeric(cutoff) || cutoff < 0) {
    stop("Cutoff must be a non-negative number, got: ", cutoff)
  }

  if (!is.numeric(dalton) || dalton <= 0 || !is.numeric(ppm) || ppm <= 0) {
    stop(
      "Dalton and PPM tolerances must be positive numbers ",
      "(dalton: ",
      dalton,
      ", ppm: ",
      ppm,
      ")"
    )
  }

  # Validate polarity
  if (!is.na(polarity) && !polarity %in% c("pos", "neg")) {
    stop("Polarity must be 'pos', 'neg', or NA, got: ", polarity)
  }

  # Validate file path (I/O check - more expensive)
  if (!is.character(file) || length(file) != 1L) {
    stop("file must be a single character string")
  }

  if (!file.exists(file)) {
    stop("Spectra file not found: ", file)
  }

  # Import Spectra ----

  log_info("Importing spectra from: %s", file)
  log_debug(
    "Parameters: cutoff=%f2, dalton=%f2, ppm=%f2, polarity=%s",
    cutoff,
    dalton,
    ppm,
    ifelse(test = is.na(polarity), yes = "all", no = polarity)
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

  # log_trace("Detected file format: %s", file_ext)

  # Import spectra based on file format
  spectra <- tryCatch(
    {
      switch(
        EXPR = file_ext,
        "mgf" = {
          log_debug("Reading MGF file...")
          read_mgf_opti(f = file) |>
            Spectra::Spectra()
        },
        "msp" = {
          log_debug("Reading MSP file...")
          MsBackendMsp::readMsp(f = file) |>
            Spectra::Spectra()
        },
        "rds" = {
          log_debug("Reading RDS file...")
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
      log_error("Failed to import spectra: %s", conditionMessage(e))
      stop("Failed to import spectra from ", file, ": ", conditionMessage(e))
    }
  )

  n_initial <- length(spectra)
  log_info("Loaded %d spectra from file", n_initial)

  # Early exit for empty files
  if (n_initial == 0L) {
    log_warn("No spectra found in file")
    return(spectra)
  }

  # Harmonize Metadata Fields ----

  # Validate precursor charges
  if (0L %in% spectra@backend@spectraData$precursorCharge) {
    n_unknown <- sum(spectra@backend@spectraData$precursorCharge == 0L)
    log_warn(
      "Found %d spectra with precursorCharge = 0 (unknown polarity - should be avoided in practice)",
      n_unknown
    )
  }

  # Harmonize legacy metadata fields to standard names
  spec_cols <- colnames(spectra@backend@spectraData)
  if ("MSLEVEL" %in% spec_cols) {
    spectra$msLevel <- as.integer(spectra$MSLEVEL)
  }
  if ("MS_LEVEL" %in% spec_cols) {
    spectra$msLevel <- as.integer(spectra$MS_LEVEL)
  }
  if ("PRECURSOR_MZ" %in% spec_cols) {
    spectra$precursorMz <- as.numeric(spectra$PRECURSOR_MZ)
  }
  if ("spectrum_id" %in% spec_cols) {
    spectra$spectrum_id <- as.character(spectra$spectrum_id)
  }

  # Filter Spectra ----

  # Filter to MS2 spectra only if msLevel available
  if ("msLevel" %in% spec_cols) {
    n_before <- length(spectra)
    spectra <- Spectra::filterMsLevel(object = spectra, msLevel. = 2L)
    n_after <- length(spectra)
    n_removed <- n_before - n_after
    if (n_removed > 0L) {
      log_debug(
        "Filtered to MS2 spectra: %d -> %d (%d removed)",
        n_before,
        n_after,
        n_removed
      )
    }
  }

  # Handle MassBank-specific MS level field if present
  if ("Spectrum_type" %in% colnames(spectra@backend@spectraData)) {
    n_before <- length(spectra)
    spectra <- spectra[spectra@backend@spectraData$Spectrum_type == "MS2"]
    n_after <- length(spectra)
    log_debug(
      "Filtered to MS2 spectra (MassBank): %d -> %d spectra",
      n_before,
      n_after
    )
  }

  # Filter by polarity if specified (fallback to precursorCharge only)
  if (!is.na(polarity)) {
    n_before <- length(spectra)
    charge_values <- if (polarity == "pos") {
      c(0L, 1L, 2L, 3L)
    } else {
      c(0L, -1L, -2L, -3L)
    }
    spectra <- Spectra::filterPrecursorCharge(
      object = spectra,
      z = charge_values
    )
    n_after <- length(spectra)
    log_debug(
      "Filtered to %s polarity: %d -> %d spectra",
      polarity,
      n_before,
      n_after
    )
  }

  # Combine replicate spectra if requested
  if (combine) {
    n_before <- length(spectra)
    if ("FEATURE_ID" %in% colnames(spectra@backend@spectraData)) {
      log_debug("Combining replicate spectra by FEATURE_ID")
      spectra <- spectra |>
        Spectra::combineSpectra(
          f = spectra$FEATURE_ID,
          tolerance = dalton,
          ppm = ppm
        ) |>
        Spectra::combinePeaks(tolerance = dalton, ppm = ppm)
      n_after <- length(spectra)
      log_debug("Combined replicates: %d -> %d spectra", n_before, n_after)
    } else if ("SLAW_ID" %in% colnames(spectra@backend@spectraData)) {
      log_debug("Combining replicate spectra by SLAW_ID")
      spectra <- spectra |>
        Spectra::combineSpectra(
          f = spectra$SLAW_ID,
          tolerance = dalton,
          ppm = ppm
        ) |>
        Spectra::combinePeaks(tolerance = dalton, ppm = ppm)
      n_after <- length(spectra)
      log_debug("Combined replicates: %d -> %d spectra", n_before, n_after)
    } else {
      # log_trace(
      #  "No replicate grouping field found, skipping combination"
      # )
    }
  }

  # Sanitize spectra if requested
  if (sanitize) {
    n_before <- length(spectra)
    log_debug("Sanitizing spectra (cutoff=%f2)", cutoff)
    spectra <- sanitize_spectra(
      spectra = spectra,
      cutoff = cutoff,
      dalton = dalton,
      ppm = ppm
    )
    n_after <- length(spectra)
    log_debug("Sanitization complete: %d -> %d spectra", n_before, n_after)
  }

  log_info(
    "Import complete: %d spectra ready for analysis",
    length(spectra)
  )

  return(spectra)
}
