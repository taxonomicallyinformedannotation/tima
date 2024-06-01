#' @title Cleanup spectra
#'
#' @description This function cleans up spectra (keeps only merged MS2)
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
    Spectra::reduceSpectra()
  if ("MSLEVEL" %in% colnames(spectra@backend@spectraData)) {
    spectra <- spectra[spectra$MSLEVEL == 2]
  }
  if ("FEATURE_ID" %in% colnames(spectra@backend@spectraData)) {
    message("Combining spectra in case...")
    spectra <- spectra |>
      Spectra::combineSpectra(f = spectra$FEATURE_ID)
  }
  return(spectra)
}
