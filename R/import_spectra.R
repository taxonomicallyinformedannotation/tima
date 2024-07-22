import::from(MsBackendMgf, readMgfSplit, .into = environment())
import::from(MsBackendMsp, readMsp, .into = environment())
import::from(Spectra, Spectra, .into = environment())
import::from(stringi, stri_replace_all_regex, .into = environment())

#' @title Import spectra
#'
#' @description This function imports spectra from a file (.mgf or .sqlite)
#'
#' @importFrom MsBackendMgf readMgfSplit
#' @importFrom MsBackendMsp readMsp
#' @importFrom Spectra Spectra
#' @importFrom stringi stri_replace_all_regex
#'
#' @include cleanup_spectra.R
#'
#' @param file File path of the spectrum file to be imported
#'
#' @return Spectra object containing the imported spectra
#'
#' @export
#'
#' @examples NULL
import_spectra <- function(file) {
  file_ext <-
    stri_replace_all_regex(
      str = file,
      pattern = ".*\\.",
      replacement = "",
      vectorize_all = FALSE
    )

  switch(
    EXPR = file_ext,
    "mgf" = {
      readMgfSplit(f = file) |>
        Spectra() |>
        cleanup_spectra()
    },
    "msp" = {
      readMsp(f = file) |>
        Spectra() |>
        cleanup_spectra()
    },
    # "sqlite" = {
    #   CompDb(x = file) |>
    #     Spectra() |>
    #     setBackend(MsBackendMemory())
    # },
    "rds" = {
      readRDS(file = file) |>
        data.frame() |>
        Spectra() |>
        cleanup_spectra()
    }
  )
}
