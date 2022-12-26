#' @title Import spectra
#'
#' @description TODO
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

  switch(
    EXPR = file_ext,
    "mgf" = {
      MsBackendMgf::readMgf(file) |>
        Spectra::Spectra()
    },
    "sqlite" = {
      CompoundDb::CompDb(file) |>
        CompoundDb::Spectra()
    }
  )
}
