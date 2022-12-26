#' @title Import spectra
#'
#' @param file the file path of the spectrum file to be imported
#'
#' @return a Spectra object containing the imported spectra
#'
#' @export
#'
#' @importFrom CompoundDb CompDb Spectra
#' @importFrom MsBackendMgf readMgf
#' @importFrom Spectra Spectra
#' @importFrom stringr fixed str_remove
#'
#' @examples TODO
import_spectra <- function(file) {
  file_ext <- stringr::str_remove(string = file, pattern = stringr::fixed(".*\\."))

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
