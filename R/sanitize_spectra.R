#' @title Sanitize spectra
#'
#' @description This function sanitizes spectra
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
    # BiocParallel::bplapply(FUN = Spectra:::.peaks_remove_fft_artifact)

    spectra <- spectra |>
      Spectra::dropNaSpectraVariables() |>
      Spectra::filterFourierTransformArtefacts() |> # fixed in Spectra 1.10.3
      Spectra::filterIntensity(intensity = c(cutoff, Inf)) |>
      Spectra::filterIntensity(intensity = keep_peaks, prop = ratio) |>
      Spectra::applyProcessing()

    # spectra <- spectra |>
    #   Spectra::filterIntensity(
    #     intensity = function(x) {
    #       ## eventually go to 25%
    #       x <- x > stats::quantile(x)[1]
    #     }
    #   ) |>
    #   Spectra::applyProcessing()

    spectra <- spectra[BiocParallel::bplapply(X = spectra@backend@peaksData, FUN = length) >= fragments * 2]

    return(spectra)
  }
