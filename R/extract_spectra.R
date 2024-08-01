import::from(tidytable, across, .into = environment())
import::from(tidytable, any_of, .into = environment())
import::from(tidytable, as_tidytable, .into = environment())
import::from(tidytable, everything, .into = environment())
import::from(tidytable, group_by, .into = environment())
import::from(tidytable, mutate, .into = environment())
import::from(tidytable, rename, .into = environment())
import::from(tidytable, select, .into = environment())

#' @title Extract spectra from a Spectra object
#'
#' @description This function extracts spectra from a `Spectra`object
#'
#' @importFrom tidytable across
#' @importFrom tidytable any_of
#' @importFrom tidytable as_tidytable
#' @importFrom tidytable everything
#' @importFrom tidytable group_by
#' @importFrom tidytable mutate
#' @importFrom tidytable rename
#' @importFrom tidytable select
#'
#' @param object Object of class Spectra
#'
#' @return Data frame containing spectra data
#'
#' @export
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
    as_tidytable()

  ## Add 'mz' and 'intensity' columns from peaks data
  spectra$mz <- lapply(object@backend@peaksData, function(peakData) {
    peakData[, 1]
  })
  spectra$intensity <- lapply(object@backend@peaksData, function(peakData) {
    peakData[, 2]
  })

  ## Synonyms issue
  # spectra <- spectra |>
  # group_by(c(-any_of("synonym"))) |>
  # reframe(across(
  # .cols = where(is.list),
  # .fns = as.character
  # )) |>
  # ungroup()

  ## Columns types issue
  spectra <- spectra |>
    mutate(across(.cols = any_of(incoherent_logical), .fns = as.logical)) |>
    mutate(across(.cols = any_of(incoherent_integer), .fns = as.integer)) |>
    mutate(across(.cols = any_of(incoherent_numeric), .fns = as.numeric))

  ## Select all columns except those specified in 'incoherent_colnames',
  ## and rename the remaining columns using the names in 'incoherent_colnames'
  incoherent_colnames <-
    incoherent_colnames[unname(incoherent_colnames) %in% colnames(spectra)]
  spectra <- spectra |>
    select(-c(any_of(names(
      incoherent_colnames
    )))) |>
    rename(any_of(incoherent_colnames))

  return(spectra)
}
