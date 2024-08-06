import::from(MsBackendMgf, readMgf, .into = environment())
# TODO Change as soon as R 4.4.0 becomes oldrel
# import::from(MsBackendMgf, readMgfSplit, .into = environment())
import::from(MsBackendMsp, readMsp, .into = environment())
import::from(Spectra, Spectra, .into = environment())
import::from(stringi, stri_replace_all_regex, .into = environment())

#' @title Import spectra
#'
#' @description This function imports spectra from a file (.mgf or .sqlite)
#'
#' @importFrom MsBackendMgf readMgf
#' @importFrom MsBackendMsp readMsp
#' @importFrom Spectra Spectra
#' @importFrom stringi stri_replace_all_regex
#'
#' @include sanitize_spectra.R
#'
#' @param file File path of the spectrum file to be imported
#' @param cutoff Absolute minimal intensity
#' @param dalton Dalton tolerance
#' @param polarity Polarity
#' @param ppm PPM tolerance
#' @param sanitize Flag indicating whether to sanitize. Default TRUE
#'
#' @return Spectra object containing the imported spectra
#'
#' @export
#'
#' @examples NULL
import_spectra <- function(file,
                           cutoff = 0,
                           dalton = 0.01,
                           polarity = NA,
                           ppm = 10,
                           sanitize = TRUE) {
  file_ext <-
    stri_replace_all_regex(
      str = file,
      pattern = ".*\\.",
      replacement = "",
      vectorize_all = FALSE
    )

  spectra <- switch(
    EXPR = file_ext,
    "mgf" = {
      readMgf(f = file) |>
        # TODO Change as soon as R 4.4.0 becomes oldrel
        # readMgfSplit(f = file) |>
        Spectra()
    },
    "msp" = {
      readMsp(f = file) |>
        Spectra()
    },
    # "sqlite" = {
    #   CompDb(x = file) |>
    #     Spectra() |>
    #     setBackend(MsBackendMemory())
    # },
    "rds" = {
      readRDS(file = file) |>
        data.frame() |>
        Spectra()
    }
  )
  if (sanitize) {
    spectra <- spectra |>
      sanitize_spectra(
        cutoff = cutoff,
        dalton = dalton,
        polarity = polarity,
        ppm = ppm
      )
  }
  return(spectra)
}
