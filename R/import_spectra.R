#' @title Import spectra
#'
#' @param file TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom CompoundDb CompDb Spectra
#' @importFrom MsBackendMgf readMgf
#' @importFrom Spectra Spectra
#'
#' @examples TODO
import_spectra <- function(file) {
  switch(
    EXPR = gsub(
      pattern = ".*\\.",
      replacement = "",
      x = file
    ),
    "mgf" = {
      file |>
        MsBackendMgf::readMgf() |>
        Spectra::Spectra()
    },
    "sqlite" = {
      file |>
        CompoundDb::CompDb() |>
        CompoundDb::Spectra()
    }
  )
}
