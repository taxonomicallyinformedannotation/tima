#' @title Sanitize spectra
#'
#' @description This function sanitizes spectra
#'
#' @param spectra Spectra object
#' @param cutoff Absolute minimal intensity
#' @param dalton Dalton tolerance
#' @param polarity Polarity
#' @param ppm PPM tolerance
#'
#' @return The sanitized spectra
#'
#' @export
#'
#' @examples
#' data.frame(
#'   FEATURE_ID = c("FT001", "FT002", "FT003"),
#'   mz = c(list(123.4567, 234.5678, 345.6789))
#' ) |>
#'   Spectra::Spectra() |>
#'   sanitize_spectra()
sanitize_spectra <-
  function(spectra,
           cutoff = 0,
           dalton = 0.01,
           polarity = NA,
           ppm = 10) {
    log_debug("Applying sanitization of the spectra")

    if ("msLevel" %in% colnames(spectra@backend@spectraData)) {
      message("Filtering MS2 only")
      spectra <- spectra |>
        Spectra::filterMsLevel(2L)
    }

    if (!is.na(polarity)) {
      spectra <- spectra |>
        Spectra::filterPrecursorCharge(z = if (polarity == "pos") {
          c(1, 2, 3)
        } else {
          c(-1, -2, -3)
        })
    }

    spectra <- spectra |>
      Spectra::dropNaSpectraVariables() |>
      Spectra::reduceSpectra(tolerance = dalton, ppm = ppm) |>
      Spectra::filterFourierTransformArtefacts() |> # fixed in Spectra 1.10.3
      Spectra::filterIntensity(intensity = c(cutoff, Inf)) |>
      Spectra::filterPrecursorPeaks(
        tolerance = dalton,
        ppm = ppm,
        mz = c(">=")
      ) |>
      Spectra::scalePeaks()

    if ("FEATURE_ID" %in% colnames(spectra@backend@spectraData)) {
      message("Combining spectra in case...")
      spectra <- spectra |>
        Spectra::combineSpectra(f = spectra$FEATURE_ID)
    }

    spectra <- spectra |>
      Spectra::filterEmptySpectra()

    return(spectra)
  }
