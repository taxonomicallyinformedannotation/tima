import::from(tidytable, fread, .into = environment())
import::from(utils, unzip, .into = environment())

#' @title Read from SIRIUS zip
#'
#' @description This function reads files from Sirius compressed workspace
#'
#' @importFrom tidytable fread
#' @importFrom utils unzip
#'
#' @param sirius_zip Compressed directory containing the Sirius results
#' @param file File to be read
#'
#' @return NULL
#'
#' @examples NULL
read_from_sirius_zip <- function(sirius_zip, file) {
  sirius_no_zip <- sirius_zip |>
    gsub(
      pattern = ".zip",
      replacement = "",
      fixed = TRUE
    )
  if (sirius_zip != sirius_no_zip) {
    temp <- tempdir()
    sirius_dir <- file.path(temp, sirius_no_zip |>
      basename())
    if (!file.exists(sirius_dir)) {
      unzip(sirius_zip, exdir = temp)
    }
  } else {
    sirius_dir <- sirius_no_zip
  }

  fread(
    file = file.path(sirius_dir, file),
    na.strings = c("", "NA"),
    colClasses = "character"
  )
}
