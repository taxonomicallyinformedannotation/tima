#' @title Sanitize spectra
#'
#' @description This function sanitizes spectra
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
           ratio = 1000,
           cutoff = qutoff,
           fragments = 2) {
    log_debug("Applying initial filters to query spectra")

    spectra <- spectra |>
      Spectra::dropNaSpectraVariables() |>
      Spectra::filterIntensity(intensity = c(cutoff, Inf)) |>
      Spectra::filterIntensity(intensity = keep_peaks, prop = ratio) |>
      Spectra::applyProcessing()

    lengths <- lapply(spectra@backend@peaksData, length)
    spectra <- spectra[lengths >= fragments * 2]

    return(spectra)
  }
