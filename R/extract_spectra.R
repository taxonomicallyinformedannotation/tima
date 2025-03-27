#' @title Extract spectra from a Spectra object
#'
#' @description This function extracts spectra from a `Spectra`object
#'
#' @param object Object of class Spectra
#'
#' @return Data frame containing spectra data
#'
#' @examples NULL
extract_spectra <- function(object) {
  ## issues
  incoherent_colnames <- c(
    ms_level = "msLevel",
    precursor_intensity = "precursorIntensity",
    precursorMz = "PrecursorMZ"
  )
  incoherent_logical <- c("predicted")
  incoherent_integer <- c("spectrum_id")
  incoherent_numeric <- c("PrecursorMZ")

  ## Extract spectra data and transform it into a data frame
  spectra <- object@backend@spectraData |>
    data.frame() |>
    tidytable::as_tidytable()

  ## Add 'mz' and 'intensity' columns from peaks data
  spectra$mz <- purrr::map(
    .x = object@backend@peaksData,
    .f = function(peakData) {
      peakData[, 1]
    }
  )
  spectra$intensity <- purrr::map(
    .x = object@backend@peaksData,
    .f = function(peakData) {
      peakData[, 2]
    }
  )

  ## Synonyms issue
  # spectra <- spectra |>
  # tidytable::group_by(c(-tidyselect::any_of("synonym"))) |>
  # tidytable::reframe(tidytable::across(
  # .cols = tidyselect::where(is.list),
  # .fns = as.character
  # )) |>
  # tidytable::ungroup()

  ## Columns types issue
  spectra <- spectra |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::any_of(incoherent_logical),
      .fns = as.logical
    )) |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::any_of(incoherent_integer),
      .fns = as.integer
    )) |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::any_of(incoherent_numeric),
      .fns = as.numeric
    ))

  ## Select all columns except those specified in 'incoherent_colnames',
  ## and rename the remaining columns using the names in 'incoherent_colnames'
  incoherent_colnames <-
    incoherent_colnames[unname(incoherent_colnames) %in% colnames(spectra)]
  spectra <- spectra |>
    tidytable::select(
      -c(tidyselect::any_of(names(
        incoherent_colnames
      )))
    ) |>
    tidytable::rename(tidyselect::any_of(incoherent_colnames))

  return(spectra)
}
