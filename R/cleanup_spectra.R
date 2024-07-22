import::from(Spectra, applyProcessing, .into = environment())
import::from(Spectra, combineSpectra, .into = environment())
import::from(Spectra, filterEmptySpectra, .into = environment())
import::from(Spectra, reduceSpectra, .into = environment())

#' @title Cleanup spectra
#'
#' @description This function cleans up spectra (keeps only merged MS2)
#'
#' @importFrom Spectra applyProcessing
#' @importFrom Spectra combineSpectra
#' @importFrom Spectra filterEmptySpectra
#' @importFrom Spectra reduceSpectra
#'
#' @param spectra Spectra object to clean
#'
#' @return Spectra object containing the imported spectra
#'
#' @export
#'
#' @examples NULL
cleanup_spectra <- function(spectra) {
  spectra <- spectra |>
    reduceSpectra() |>
    applyProcessing()
  if ("MSLEVEL" %in% colnames(spectra@backend@spectraData)) {
    spectra <- spectra[spectra$MSLEVEL == 2]
  }
  if ("FEATURE_ID" %in% colnames(spectra@backend@spectraData)) {
    message("Combining spectra in case...")
    spectra <- spectra |>
      combineSpectra(f = spectra$FEATURE_ID)
  }
  spectra <- spectra |>
    filterEmptySpectra()
  return(spectra)
}
