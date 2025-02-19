#' @title Export spectra RDS
#'
#' @description This function exports spectra in RDS
#'
#' @include create_dir.R
#'
#' @param file File where spectra will be exported.
#' @param spectra The spectra object where spectra are stored
#'
#' @return NULL
#'
#' @examples NULL
export_spectra_rds <- function(file, spectra) {
  if (length(spectra[!is.na(spectra$compound_id)]) != 0) {
    create_dir(export = file)
    spectra |>
      saveRDS(file = file)
  }
}
