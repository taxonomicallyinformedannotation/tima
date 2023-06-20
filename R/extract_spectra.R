utils::globalVariables(c("group"))

#' @title Extract spectra from a Spectra object
#'
#' @description This function extracts spectra from a `Spectra`object
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
    precursor_intensity = "precursorIntensity"
  )
  incoherent_logical <- c("predicted")
  incoherent_integer <- c("spectrum_id")

  ## Extract peaks data and transform it into a data frame
  peaks <- object |>
    Spectra::peaksData() |>
    data.frame() |>
    dplyr::group_by(group) |>
    dplyr::summarize(dplyr::across(dplyr::everything(),
      .fns = list
    ))

  ## Extract spectra data and transform it into a data frame
  spectra <- object |>
    Spectra::spectraData() |>
    data.frame()

  ## Add 'mz' and 'intensity' columns from peaks data
  spectra$mz <- peaks$mz
  spectra$intensity <- peaks$intensity

  ## Synonyms issue
  spectra <- spectra |>
    dplyr::group_by(dplyr::across(c(-dplyr::any_of("synonym")))) |>
    dplyr::summarize(dplyr::across(.cols = dplyr::where(is.list), .fns = as.character)) |>
    dplyr::ungroup()

  ## Columns types issue
  spectra <- spectra |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::any_of(incoherent_logical),
      .fns = as.logical
    )) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::any_of(incoherent_integer),
      .fns = as.integer
    ))

  ## Select all columns except those specified in 'incoherent_colnames', and rename the remaining columns using the names in 'incoherent_colnames'
  spectra <- spectra |>
    dplyr::select(-c(dplyr::any_of(incoherent_colnames))) |>
    dplyr::rename(dplyr::any_of(incoherent_colnames))

  return(spectra)
}
