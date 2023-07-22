utils::globalVariables(
  c(
    "compound_id"
  )
)

#' @title Export spectra 2
#'
#' @description This function export spectra.
#'
#' @include export_spectra.R
#'
#' @param file File where spectra will be exported. Can be '.mgf' or '.sqlite'
#' @param spectra The spectra object where spectra are stored
#' @param meta Optional. Metadata as in 'CompoundDb' package
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
export_spectra_2 <- function(file,
                             spectra,
                             meta) {
  if (nrow(spectra |>
    dplyr::filter(!is.na(compound_id))) != 0) {
    log_debug("Exporting")
    create_dir(export = file)
    spectra |>
      export_spectra(
        file = file,
        metad = meta
      )
  }
}
