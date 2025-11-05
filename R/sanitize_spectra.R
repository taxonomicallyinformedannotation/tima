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
  # Validate inputs
  if (!inherits(spectra, "Spectra")) {
    stop("Input must be a Spectra object from the Spectra package")
  }

  if (cutoff < 0) {
    stop("Cutoff intensity must be non-negative, got: ", cutoff)
  }

  if (dalton <= 0 || ppm <= 0) {
    stop("Tolerance values must be positive (dalton: {dalton}, ppm: {ppm})")
  }

  n_initial <- length(spectra)
  logger::log_info("Sanitizing {n_initial} spectra (cutoff: {cutoff})")

  # Apply sequential sanitization steps
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

  # Filter spectra with fewer than 3 peaks
  n_before <- length(spectra)
  spectra <- spectra[lengths(spectra@backend@peaksData) > 2L]
  n_removed_peaks <- n_before - length(spectra)
  if (n_removed_peaks > 0L) {
    logger::log_debug("Removed {n_removed_peaks} spectra with <= 2 peaks")
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
    logger::log_debug("Removed {n_nan} spectra containing NaN values")
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
    logger::log_debug("Removed {n_null} NULL spectra")
  }

  n_final <- length(spectra)
  n_total_removed <- n_initial - n_final
  pct_retained <- round(100 * n_final / n_initial, 1)
  logger::log_info(
    "Sanitization complete: {n_final}/{n_initial} spectra retained ({pct_retained}%, {n_total_removed} removed)"
  )

  return(spectra)
}
