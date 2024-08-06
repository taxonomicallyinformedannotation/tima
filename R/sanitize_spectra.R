import::from(Spectra, addProcessing, .into = environment())
import::from(Spectra, applyProcessing, .into = environment())
import::from(Spectra, combineSpectra, .into = environment())
import::from(Spectra, dropNaSpectraVariables, .into = environment())
import::from(Spectra, filterEmptySpectra, .into = environment())
import::from(Spectra, filterFourierTransformArtefacts, .into = environment())
import::from(Spectra, filterIntensity, .into = environment())
import::from(Spectra, filterMsLevel, .into = environment())
import::from(Spectra, filterPrecursorCharge, .into = environment())
import::from(Spectra, filterPrecursorPeaks, .into = environment())
import::from(Spectra, reduceSpectra, .into = environment())
import::from(Spectra, scalePeaks, .into = environment())

#' @title Sanitize spectra
#'
#' @description This function sanitizes spectra
#'
#' @importFrom Spectra addProcessing
#' @importFrom Spectra applyProcessing
#' @importFrom Spectra combineSpectra
#' @importFrom Spectra dropNaSpectraVariables
#' @importFrom Spectra filterEmptySpectra
#' @importFrom Spectra filterFourierTransformArtefacts
#' @importFrom Spectra filterIntensity
#' @importFrom Spectra filterMsLevel
#' @importFrom Spectra filterPrecursorCharge
#' @importFrom Spectra filterPrecursorPeaks
#' @importFrom Spectra reduceSpectra
#' @importFrom Spectra scalePeaks
#'
#' @param spectra Spectra object
#' @param cutoff Absolute minimal intensity
#' @param dalton Dalton tolerance
#' @param polarity Polarity
#' @param ppm PPM tolerance
#' @param ratio Minimal ratio to the max peak
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
sanitize_spectra <-
  function(spectra,
           cutoff = 0,
           dalton = 0.01,
           polarity = NA,
           ppm = 10,
           ratio = 10000) {
    log_debug("Applying sanitization of the spectra")

    if ("msLevel" %in% colnames(spectra@backend@spectraData)) {
      message("Filtering MS2 only")
      spectra <- spectra |>
        filterMsLevel(2L)
    }

    if (!is.na(polarity)) {
      spectra <- spectra |>
        filterPrecursorCharge(z = if (polarity == "pos") {
          c(1, 2, 3)
        } else {
          c(-1, -2, -3)
        })
    }

    spectra <- spectra |>
      dropNaSpectraVariables() |>
      reduceSpectra(tolerance = dalton, ppm = ppm) |>
      filterFourierTransformArtefacts() |> # fixed in Spectra 1.10.3
      filterIntensity(intensity = c(cutoff, Inf)) |>
      filterPrecursorPeaks(
        tolerance = dalton,
        ppm = ppm,
        mz = c(">=")
      ) |>
      scalePeaks() |>
      filterIntensity(intensity = c(1 / ratio, 1))

    if ("FEATURE_ID" %in% colnames(spectra@backend@spectraData)) {
      message("Combining spectra in case...")
      spectra <- spectra |>
        combineSpectra(f = spectra$FEATURE_ID)
    }

    spectra <- spectra |>
      filterEmptySpectra()

    return(spectra)
  }
