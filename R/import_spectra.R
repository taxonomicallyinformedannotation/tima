#' @title Import spectra
#'
#' @description This function imports spectra from a file (.mgf or .sqlite)
#'
#' @include read_mgf_opti.R
#' @include sanitize_spectra.R
#'
#' @param file File path of the spectrum file to be imported
#' @param cutoff Absolute minimal intensity
#' @param dalton Dalton tolerance
#' @param polarity Polarity
#' @param ppm PPM tolerance
#' @param sanitize Flag indicating whether to sanitize. Default TRUE
#' @param combine Flag indicating whether to combine Default TRUE
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
import_spectra <- function(
  file,
  cutoff = 0,
  dalton = 0.01,
  polarity = NA,
  ppm = 10,
  sanitize = TRUE,
  combine = TRUE
) {
  file_ext <- file |>
    stringi::stri_replace_all_regex(
      pattern = ".*\\.",
      replacement = "",
      vectorize_all = FALSE
    ) |>
    ## See https://github.com/MassBank/MassBank-data/issues/299
    stringi::stri_replace_all_regex(
      pattern = "_.*",
      replacement = "",
      vectorize_all = FALSE
    )

  spectra <- switch(
    EXPR = file_ext,
    "mgf" = {
      read_mgf_opti(f = file) |>
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

  if (0 %in% spectra@backend@spectraData$precursorCharge) {
    logger::log_warn("Spectra with charge = 0 found in the MGF.")
    logger::log_warn("Considering them even if this should not be the case.")
  }

  ## Fix needed
  if ("MSLEVEL" %in% colnames(spectra@backend@spectraData)) {
    logger::log_trace("Harmonizing names")
    spectra$msLevel <- spectra$MSLEVEL |>
      as.integer()
  }
  if ("MS_LEVEL" %in% colnames(spectra@backend@spectraData)) {
    logger::log_trace("Harmonizing names")
    spectra$msLevel <- spectra$MS_LEVEL |>
      as.integer()
  }

  if ("PRECURSOR_MZ" %in% colnames(spectra@backend@spectraData)) {
    logger::log_trace("Harmonizing names")
    spectra$precursorMz <- spectra$PRECURSOR_MZ |>
      as.numeric()
  }

  if ("spectrum_id" %in% colnames(spectra@backend@spectraData)) {
    logger::log_trace("Harmonizing spectrum id")
    spectra$spectrum_id <- spectra$spectrum_id |>
      as.character()
  }

  if ("msLevel" %in% colnames(spectra@backend@spectraData)) {
    logger::log_trace("Filtering MS2 only")
    spectra <- spectra |>
      Spectra::filterMsLevel(2L)
  }
  ## In MassBank
  if ("Spectrum_type" %in% colnames(spectra@backend@spectraData)) {
    logger::log_trace("Filtering MS2 only")
    spectra <- spectra[spectra@backend@spectraData$Spectrum_type == "MS2"]
  }

  if (!is.na(polarity)) {
    spectra <- spectra |>
      Spectra::filterPrecursorCharge(
        ## COMMENT: Considering 0 as both polarities...suboptimal
        ## But allows reading broken MGFs
        z = if (polarity == "pos") {
          c(0, 1, 2, 3)
        } else {
          c(0, -1, -2, -3)
        }
      )
  }

  if (combine) {
    if ("FEATURE_ID" %in% colnames(spectra@backend@spectraData)) {
      logger::log_trace("Combining spectra in case")
      spectra <- spectra |>
        Spectra::combineSpectra(
          f = spectra$FEATURE_ID,
          tolerance = dalton,
          ppm = ppm
        ) |>
        Spectra::combinePeaks(tolerance = dalton, ppm = ppm)
    }
    if ("SLAW_ID" %in% colnames(spectra@backend@spectraData)) {
      logger::log_trace("Combining spectra in case")
      spectra <- spectra |>
        Spectra::combineSpectra(
          f = spectra$SLAW_ID,
          tolerance = dalton,
          ppm = ppm
        ) |>
        Spectra::combinePeaks(tolerance = dalton, ppm = ppm)
    }
  }
  if (sanitize) {
    spectra <- spectra |>
      sanitize_spectra(
        cutoff = cutoff,
        dalton = dalton,
        ppm = ppm
      )
  }
  return(spectra)
}
