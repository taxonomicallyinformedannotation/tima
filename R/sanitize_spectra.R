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
    stop("Cutoff intensity must be non-negative")
  }

  if (dalton <= 0 || ppm <= 0) {
    stop("Tolerance values must be positive")
  }

  logger::log_trace("Sanitizing ", length(spectra), " spectra")

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

  logger::log_trace("Filtering empty and invalid spectra")

  # Filter spectra with fewer than 3 peaks
  spectra <- spectra[lengths(spectra@backend@peaksData) > 2L]

  # Filter spectra containing NaN values
  # Using vapply for type safety and better performance
  has_nan <- vapply(
    spectra@backend@peaksData,
    function(x) any(is.nan(x)),
    logical(1L)
  )
  spectra <- spectra[!has_nan]

  # Filter NULL spectra (workaround for edge cases)
  has_null <- vapply(
    spectra@backend@peaksData,
    is.null,
    logical(1L)
  )
  spectra <- spectra[!has_null]

  logger::log_info("Retained ", length(spectra), " valid spectra after sanitization")

  return(spectra)
}
