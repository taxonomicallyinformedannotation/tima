#' @title Import spectra
#'
#' @description This function imports spectra from a file (.mgf or .sqlite)
#'
#' @param file File path of the spectrum file to be imported
#'
#' @return Spectra object containing the imported spectra
#'
#' @export
#'
#' @importFrom CompoundDb CompDb Spectra
#' @importFrom MsBackendMgf readMgf
#' @importFrom Spectra Spectra
#' @importFrom stringr str_remove
#'
#' @examples NULL
import_spectra <- function(file) {
  file_ext <- stringr::str_remove(string = file, pattern = ".*\\.")

  switch(EXPR = file_ext,
    "mgf" = {
      spectra <- MsBackendMgf::readMgf(f = file, msLevel = 2L) |>
        Spectra::Spectra()
      ## Steps needed because of new mzmine3 MGF produced for SIRIUS
      if ("MSLEVEL" %in% Spectra::spectraVariables(spectra)) {
        spectra <- spectra[spectra$MSLEVEL == 2]
      }
      if (all(is.na(spectra@backend@spectraData$acquisitionNum))) {
        spectra@backend@spectraData$acquisitionNum <-
          spectra@backend@spectraData$FEATURE_ID
      }
      if (all(is.na(spectra@backend@spectraData$rtime)) &
        "RT" %in% Spectra::spectraVariables(spectra)) {
        spectra@backend@spectraData$rtime <-
          as.numeric(spectra@backend@spectraData$RT)
      }
      return(spectra)
    },
    "sqlite" = {
      CompoundDb::CompDb(x = file) |>
        CompoundDb::Spectra()
    }
  )
}
