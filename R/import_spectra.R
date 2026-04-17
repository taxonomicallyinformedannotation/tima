#' @title Import spectra
#'
#' @description This function imports mass spectra from various file formats
#'     (.mgf, .msp, .rds), harmonizes metadata field names, filters by MS level
#' and polarity, optionally combines replicate spectra, and sanitizes peak data.
#'
#' @include read_mgf_opti.R
#' @include sanitize_spectra.R
#'
#' @param file Character string path to the spectrum file (.mgf, .msp, or .rds)
#' @param cutoff Numeric absolute minimal intensity threshold (default: NULL)
#' @param dalton Numeric Dalton tolerance for peak matching (default: 0.01)
#' @param min_fragments Integer minimum number of fragment peaks required to
#'     keep a spectrum after sanitization (default: 1)
#' @param polarity Character string for polarity filtering: "pos", "neg", or NA
#'     to keep all (default: NA)
#' @param ppm Numeric PPM tolerance for peak matching (default: 10)
#' @param sanitize Logical flag indicating whether to sanitize spectra (default:
#'     TRUE)
#' @param combine Logical flag indicating whether to combine replicate spectra
#'     (default: TRUE)
#'
#' @return Spectra object containing the imported and processed spectra
#'
#' @family mass-spectrometry
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
  cutoff = NULL,
  dalton = 0.01,
  min_fragments = 1L,
  polarity = NA,
  ppm = 10,
  sanitize = TRUE,
  combine = TRUE
) {
  # Input Validation ----

  # Validate numeric parameters first (cheapest checks)
  if (!is.numeric(dalton) || dalton <= 0 || !is.numeric(ppm) || ppm <= 0) {
    cli::cli_abort(
      "dalton and ppm tolerances must be positive numbers (dalton: {.val {dalton}}, ppm: {.val {ppm}})",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Validate polarity
  if (!is.na(polarity) && !polarity %in% c("pos", "neg")) {
    cli::cli_abort(
      "polarity must be 'pos', 'neg', or NA, got {.val {polarity}}",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Validate file path (I/O check - more expensive)
  if (!is.character(file) || length(file) != 1L) {
    cli::cli_abort(
      "{.arg file} must be a single character string",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (!is.null(cutoff) && (!is.numeric(cutoff) || cutoff < 0)) {
    cli::cli_abort(
      "cutoff intensity must be non-negative or NULL, got {.val {cutoff}}",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (!file.exists(file)) {
    cli::cli_abort(
      c(
        "spectra file not found",
        "x" = file
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Import Spectra ----

  log_info("Importing spectra from: %s", file)
  log_debug(
    "Parameters: cutoff=%.2f, dalton=%.2f, ppm=%.2f, polarity=%s",
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
        cli::cli_abort(
          "unsupported file format {.val {file_ext}}; supported formats: mgf, msp, rds",
          class = c("tima_validation_error", "tima_error"),
          call = NULL
        )
      )
    },
    error = function(e) {
      if (inherits(e, "tima_error")) {
        rlang::cnd_signal(e)
      }
      log_error("Failed to import spectra: %s", conditionMessage(e))
      cli::cli_abort(
        c(
          "failed to import spectra",
          "x" = file,
          "i" = conditionMessage(e)
        ),
        class = c("tima_runtime_error", "tima_error"),
        call = NULL
      )
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
  if ("PrecursorMZ" %in% spec_cols) {
    spectra$precursorMz <- as.numeric(spectra$PrecursorMZ)
  }
  if ("PrecursorMz" %in% spec_cols) {
    spectra$precursorMz <- as.numeric(spectra$PrecursorMz)
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
  if ("precursorMz" %in% colnames(spectra@backend@spectraData)) {
    n_before <- length(spectra)
    spectra <- spectra[!is.na(spectra@backend@spectraData$precursorMz)]
    n_after <- length(spectra)
    log_debug(
      "Filtered to MS2 spectra (MassBank): %d -> %d spectra",
      n_before,
      n_after
    )
  }
  if ("precursor" %in% colnames(spectra@backend@spectraData)) {
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
      log_info("Combining replicate spectra by FEATURE_ID")
      spectra <- spectra |>
        Spectra::combineSpectra(
          f = spectra$FEATURE_ID,
          tolerance = dalton,
          ppm = ppm
        ) |>
        Spectra::combinePeaks(tolerance = dalton, ppm = ppm)
      n_after <- length(spectra)
      log_info("Combined replicates: %d -> %d spectra", n_before, n_after)
    } else if ("SLAW_ID" %in% colnames(spectra@backend@spectraData)) {
      log_info("Combining replicate spectra by SLAW_ID")
      spectra <- spectra |>
        Spectra::combineSpectra(
          f = spectra$SLAW_ID,
          tolerance = dalton,
          ppm = ppm
        ) |>
        Spectra::combinePeaks(tolerance = dalton, ppm = ppm)
      n_after <- length(spectra)
      log_info("Combined replicates: %d -> %d spectra", n_before, n_after)
    } else {
      #  "No replicate grouping field found, skipping combination"
      # )
    }
  }

  # Sanitize spectra if requested
  if (sanitize) {
    n_before <- length(spectra)
    log_debug("Sanitizing spectra (cutoff=%.2f)", cutoff)
    spectra <- sanitize_spectra(
      spectra = spectra,
      cutoff = cutoff,
      dalton = dalton,
      min_fragments = min_fragments,
      ppm = ppm
    )
    n_after <- length(spectra)
    log_debug("Sanitization complete: %d -> %d spectra", n_before, n_after)
  }

  log_info(
    "Import complete: %d spectra ready for analysis",
    length(spectra)
  )

  spectra
}
