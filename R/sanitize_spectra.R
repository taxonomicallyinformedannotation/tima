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
#' @param deeper Deep sanitization using quantiles
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
sanitize_spectra <-
  function(spectra,
           ratio = 10000,
           cutoff = 10,
           fragments = 2,
           deeper = FALSE) {
    log_debug("Applying initial filters to query spectra")

    spectra <- spectra |>
      Spectra::dropNaSpectraVariables() |>
      Spectra::filterIntensity(intensity = c(cutoff, Inf)) |>
      Spectra::filterIntensity(intensity = keep_peaks, prop = ratio, deep = deeper) |>
      Spectra::applyProcessing()

    lengths <- lapply(spectra@backend@peaksData, length)
    spectra <- spectra[lengths >= fragments * 2]

    return(spectra)
  }
