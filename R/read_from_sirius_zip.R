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
  tidytable::fread(
    file = unzip(
      zipfile = sirius_zip,
      files = file.path(
        basename(sirius_zip) |> gsub(
          pattern = ".zip",
          replacement = "",
          fixed = TRUE
        ),
        file
      ),
      exdir = tempdir()
    ),
    na.strings = c("", "NA"),
    colClasses = "character"
  )
}
