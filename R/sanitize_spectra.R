#' @title Sanitize spectra
#'
#' @description This function sanitizes spectra
#'
#' @param spectra Spectra object
#' @param cutoff Absolute minimal intensity
#' @param dalton Dalton tolerance
#' @param polarity Polarity
#' @param ppm PPM tolerance
#'
#' @return The sanitized spectra
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data.frame(
#'   FEATURE_ID = c("FT001", "FT002", "FT003"),
#'   mz = c(list(123.4567, 234.5678, 345.6789))
#' ) |>
#'   Spectra::Spectra() |>
#'   sanitize_spectra()
#' }
sanitize_spectra <-
  function(
    spectra,
    cutoff = 0,
    dalton = 0.01,
    polarity = NA,
    ppm = 10
  ) {
    logger::log_trace("Applying sanitization of the spectra")

    ## Fix needed
    if ("MSLEVEL" %in% colnames(spectra@backend@spectraData)) {
      logger::log_trace("Harmonizing names")
      spectra$msLevel <- spectra$MSLEVEL |> as.integer()
    }
    if ("MS_LEVEL" %in% colnames(spectra@backend@spectraData)) {
      logger::log_trace("Harmonizing names")
      spectra$msLevel <- spectra$MS_LEVEL |> as.integer()
    }

    if ("PRECURSOR_MZ" %in% colnames(spectra@backend@spectraData)) {
      logger::log_trace("Harmonizing names")
      spectra$precursorMz <- spectra$PRECURSOR_MZ |> as.numeric()
    }

    if ("spectrum_id" %in% colnames(spectra@backend@spectraData)) {
      logger::log_trace("Harmonizing spectrum id")
      spectra$spectrum_id <- spectra$spectrum_id |> as.character()
    }

    if ("msLevel" %in% colnames(spectra@backend@spectraData)) {
      logger::log_trace("Filtering MS2 only")
      spectra <- spectra |>
        Spectra::filterMsLevel(2L)
    }

    if (!is.na(polarity)) {
      spectra <- spectra |>
        Spectra::filterPrecursorCharge(
          z = if (polarity == "pos") {
            c(1, 2, 3)
          } else {
            c(-1, -2, -3)
          }
        )
    }

    logger::log_trace("Reducing spectra")
    spectra <- spectra |>
      Spectra::dropNaSpectraVariables() |>
      Spectra::reduceSpectra(tolerance = dalton, ppm = ppm) |>
      Spectra::filterFourierTransformArtefacts() |> # fixed in Spectra 1.10.3
      Spectra::filterIntensity(intensity = c(cutoff, Inf)) |>
      Spectra::filterPrecursorPeaks(
        tolerance = dalton,
        ppm = ppm,
        mz = c(">=")
      ) |>
      Spectra::combinePeaks(tolerance = dalton, ppm = ppm) |>
      Spectra::scalePeaks()

    logger::log_trace("Filtering empty spectra")
    spectra <- spectra |>
      Spectra::filterEmptySpectra()
    # Fix needed as some empty spectra are else not removed
    spectra <- spectra[
      !spectra@backend@peaksData |>
        purrr::map(.f = is.nan) |>
        purrr::map(.f = any) |>
        as.character() |>
        as.logical()
    ]
    spectra <- spectra[
      !spectra@backend@peaksData |>
        purrr::map(.f = is.null) |>
        purrr::map(.f = any) |>
        as.character() |>
        as.logical()
    ]
    logger::log_info("Considering {spectra |> length()} spectra")

    return(spectra)
  }
