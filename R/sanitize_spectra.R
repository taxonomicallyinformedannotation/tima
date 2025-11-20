#' @title Sanitize spectra
#'
#' @description This function sanitizes MS spectra by removing noise, artifacts,
#'     precursor peaks, and empty spectra. It applies multiple cleaning steps
#'     including intensity filtering, peak reduction, and normalization.
#'
#' @param spectra Spectra object from the Spectra package
#' @param cutoff Numeric absolute minimal intensity threshold (default: 0).
#'     Peaks below this intensity are removed.
#' @param dalton Numeric Dalton tolerance for peak matching (default: 0.01)
#' @param ppm Numeric PPM tolerance for peak matching (default: 10)
#'
#' @return A sanitized Spectra object with noise and artifacts removed
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data.frame(
#'   FEATURE_ID = c("FT001", "FT002", "FT003"),
#'   mz = c(list(123.4567, 234.5678, 345.6789))
#' ) |>
#'   Spectra::Spectra() |>
#'   sanitize_spectra()
#' }
sanitize_spectra <- function(
  spectra,
  cutoff = 0,
  dalton = 0.01,
  ppm = 10
) {
  # Input Validation ----

  # Validate spectra object first
  if (!inherits(spectra, "Spectra")) {
    stop("Input must be a Spectra object from the Spectra package")
  }

  # Validate numeric parameters (combined checks)
  if (!is.numeric(cutoff) || cutoff < 0) {
    stop("Cutoff intensity must be non-negative, got: ", cutoff)
  }

  if (!is.numeric(dalton) || dalton <= 0 || !is.numeric(ppm) || ppm <= 0) {
    stop(
      "Tolerance values must be positive (dalton: ",
      dalton,
      ", ppm: ",
      ppm,
      ")"
    )
  }

  n_initial <- length(spectra)

  # Early exit for empty spectra
  if (n_initial == 0L) {
    logger::log_warn("No spectra to sanitize")
    return(spectra)
  }

  logger::log_info("Sanitizing {n_initial} spectra (cutoff: {cutoff})")

  # Apply Sanitization Steps ----

  # Apply sequential sanitization steps with detailed logging
  # logger::log_trace("Applying sanitization pipeline...")

  spectra <- spectra |>
    Spectra::dropNaSpectraVariables() |>
    Spectra::reduceSpectra(tolerance = dalton, ppm = ppm) |>
    Spectra::filterFourierTransformArtefacts() |>
    Spectra::filterIntensity(intensity = c(cutoff, Inf)) |>
    Spectra::filterPrecursorPeaks(
      tolerance = dalton,
      ppm = ppm,
      mz = c(">=")
    ) |>
    Spectra::combinePeaks(tolerance = dalton, ppm = ppm) |>
    Spectra::scalePeaks()

  # Post-Processing Filters ----

  # Filter spectra with fewer than 3 peaks
  n_before <- length(spectra)
  spectra <- spectra[lengths(spectra@backend@peaksData) > 2L]
  n_removed_peaks <- n_before - length(spectra)
  if (n_removed_peaks > 0L) {
    pct_removed <- round(100 * n_removed_peaks / n_before, 1)
    logger::log_debug(
      "Removed {n_removed_peaks} spectra with <= 2 peaks ({pct_removed}%)"
    )
  }

  # Filter spectra containing NaN values (using vapply for type safety)
  has_nan <- vapply(
    spectra@backend@peaksData,
    function(x) any(is.nan(x)),
    logical(1L),
    USE.NAMES = FALSE
  )
  n_nan <- sum(has_nan)
  if (n_nan > 0L) {
    spectra <- spectra[!has_nan]
    pct_nan <- round(100 * n_nan / n_before, 1)
    logger::log_debug(
      "Removed {n_nan} spectra containing NaN values ({pct_nan}%)"
    )
  }

  # Filter NULL spectra (workaround for edge cases)
  has_null <- vapply(
    spectra@backend@peaksData,
    is.null,
    logical(1L),
    USE.NAMES = FALSE
  )
  n_null <- sum(has_null)
  if (n_null > 0L) {
    spectra <- spectra[!has_null]
    pct_null <- round(100 * n_null / n_before, 1)
    logger::log_debug(
      "Removed {n_null} NULL spectra ({pct_null}%)"
    )
  }

  # Final Summary ----

  n_final <- length(spectra)
  n_total_removed <- n_initial - n_final
  pct_retained <- round(100 * n_final / n_initial, 1)

  logger::log_info(
    "Sanitization complete: {n_final}/{n_initial} spectra retained ",
    "({pct_retained}%, {n_total_removed} removed)"
  )

  return(spectra)
}
