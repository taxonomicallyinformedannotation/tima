#' @title Read from SIRIUS zip
#'
#' @description This function reads files from Sirius compressed workspace
#'
#'
#' @param sirius_zip Compressed directory containing the Sirius results
#' @param file File to be read
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
read_from_sirius_zip <- function(sirius_zip, file) {
  temp <- tempdir()
  sirius_dir <- file.path(
    temp,
    basename(sirius_zip) |> gsub(
      pattern = ".zip",
      replacement = "",
      fixed = TRUE
    )
  )
  if (!file.exists(sirius_dir)) {
    unzip(sirius_zip, exdir = temp)
  }

  tidytable::fread(
    file = file.path(
      sirius_dir,
      file
    ),
    na.strings = c("", "NA"),
    colClasses = "character"
  )
}
