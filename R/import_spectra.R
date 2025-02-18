#' @title Import spectra
#'
#' @description This function imports spectra from a file (.mgf or .sqlite)
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
#' @examples
#' \dontrun{
#' get_file(
#'   url = get_default_paths()$urls$examples$spectra_mini,
#'   export = get_default_paths()$data$source$spectra
#' )
#' import_spectra(file = get_default_paths()$data$source$spectra)
#' import_spectra(
#'   file = get_default_paths()$data$source$spectra,
#'   sanitize = FALSE
#' )
#' }
import_spectra <- function(file,
                           cutoff = 0,
                           dalton = 0.01,
                           polarity = NA,
                           ppm = 10,
                           sanitize = TRUE) {
  file_ext <-
    stringi::stri_replace_all_regex(
      str = file,
      pattern = ".*\\.",
      replacement = "",
      vectorize_all = FALSE
    )

  spectra <- switch(
    EXPR = file_ext,
    "mgf" = {
      MsBackendMgf::readMgf(f = file) |>
        # TODO Change as soon as R 4.4.0 becomes oldrel
        # MsBackendMgf::readMgfSplit(f = file) |>
        Spectra::Spectra()
    },
    "msp" = {
      MsBackendMsp::readMsp(f = file) |>
        Spectra::Spectra()
    },
    # "sqlite" = {
    #   CompDb(x = file) |>
    #     Spectra::Spectra() |>
    #     setBackend(MsBackendMemory())
    # },
    "rds" = {
      readRDS(file = file)
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
