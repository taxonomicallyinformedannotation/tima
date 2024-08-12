import::from(tidytable, filter, .into = environment())

#' @title Export spectra RDS
#'
#' @description This function exports spectra in RDS
#'
#' @importFrom tidytable filter
#'
#' @include create_dir.R
#'
#' @param file File where spectra will be exported.
#' @param spectra The spectra object where spectra are stored
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
export_spectra_rds <- function(file, spectra) {
  if (nrow(spectra |>
    filter(!is.na(compound_id))) != 0) {
    create_dir(export = file)
    spectra |>
      data.frame() |>
      saveRDS(file = file)
  }
}
