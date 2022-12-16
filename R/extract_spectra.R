#' @title Extract spectra
#'
#' @param object TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom dplyr across any_of group_by mutate rename select summarize summarize_all ungroup
#' @importFrom Spectra peaksData spectraData
#'
#' @examples TODO
extract_spectra <- function(object) {
  ## issues
  incoherent_colnames <- c(
    ms_level = "msLevel",
    precursor_intensity = "precursorIntensity"
  )
  incoherent_logical <- c("predicted")
  incoherent_integer <- c("spectrum_id")
  ##

  peaks <- object |>
    Spectra::peaksData() |>
    data.frame() |>
    dplyr::group_by(group) |>
    dplyr::summarize_all(list)
  spectra <- object |>
    Spectra::spectraData() |>
    data.frame()
  spectra$mz <- peaks$mz
  spectra$intensity <- peaks$intensity

  ## synonyms issue
  spectra <- spectra |>
    dplyr::group_by(dplyr::across(c(-dplyr::any_of("synonym")))) |>
    dplyr::summarize(dplyr::across(.cols = where(is.list), .fns = as.character)) |>
    dplyr::ungroup()

  ## columns types issue
  spectra <- spectra |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::any_of(incoherent_logical),
      .fns = as.logical
    )) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::any_of(incoherent_integer),
      .fns = as.integer
    ))

  ## columns names issue
  spectra <- spectra |>
    dplyr::select(-c(dplyr::any_of(incoherent_colnames))) |>
    dplyr::rename(dplyr::any_of(incoherent_colnames))

  return(spectra)
}
