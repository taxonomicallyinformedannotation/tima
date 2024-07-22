import::from(Spectra, applyProcessing, .into = environment())
import::from(Spectra, dropNaSpectraVariables, .into = environment())
import::from(Spectra, filterFourierTransformArtefacts, .into = environment())
import::from(Spectra, filterIntensity, .into = environment())

#' @title Sanitize spectra
#'
#' @description This function sanitizes spectra
#'
#' @importFrom Spectra applyProcessing
#' @importFrom Spectra dropNaSpectraVariables
#' @importFrom Spectra filterFourierTransformArtefacts
#' @importFrom Spectra filterIntensity
#'
#' @include keep_peaks.R
#'
#' @param spectra Spectra object
#' @param ratio Minimal ratio to the max peak
#' @param cutoff Absolute minimal intensity
#' @param fragments Minimal number of fragments
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
sanitize_spectra <-
  function(spectra,
           ratio = 10000,
           cutoff = 0,
           fragments = 3) {
    log_debug("Applying sanitization of the spectra")

    ## Not needed anymore (fixed in Spectra 1.10.3)
    ## see https://github.com/rformassspectrometry/Spectra/issues/302
    # spectra@backend@peaksData <- spectra@backend@peaksData |>
    # lapply(FUN = Spectra:::.peaks_remove_fft_artifact)

    spectra <- spectra |>
      dropNaSpectraVariables() |>
      filterFourierTransformArtefacts() |> # fixed in Spectra 1.10.3
      filterIntensity(intensity = c(cutoff, Inf)) |>
      filterIntensity(intensity = keep_peaks, prop = ratio) |>
      applyProcessing()

    # spectra <- spectra |>
    #   filterIntensity(
    #     intensity = function(x) {
    #       ## eventually go to 25%
    #       x <- x > quantile(x)[1]
    #     }
    #   ) |>
    #   applyProcessing()

    spectra <- spectra[lapply(X = spectra@backend@peaksData, FUN = length) >= fragments * 2]

    return(spectra)
  }
