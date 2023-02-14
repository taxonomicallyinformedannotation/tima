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
      MsBackendMgf::readMgf(f = file, msLevel = 2) |>
        Spectra::Spectra()
    },
    "sqlite" = {
      CompoundDb::CompDb(x = file) |>
        CompoundDb::Spectra()
    }
  )
}
