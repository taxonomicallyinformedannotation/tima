#' @title Read from SIRIUS zip
#'
#' @description This function reads files from Sirius compressed workspace
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
    ) |>
    gsub(
      pattern = ".rar",
      replacement = "",
      fixed = TRUE
    )
  if (sirius_zip != sirius_no_zip) {
    temp <- tempdir()
    sirius_dir <- file.path(temp, sirius_no_zip |>
      basename())
    if (!file.exists(sirius_dir)) {
      if (sirius_zip |> endsWith(".zip")) {
        utils::unzip(sirius_zip, exdir = temp)
      }
      if (sirius_zip |> endsWith(".rar")) {
        utils::untar(sirius_zip, exdir = temp)
        sirius_dir <- sirius_dir |>
          gsub(
            pattern = sirius_no_zip |>
              basename(),
            replacement = ""
          )
      }
    }
  } else {
    sirius_dir <- sirius_no_zip
  }

  tidytable::fread(
    file = file.path(sirius_dir, file),
    na.strings = c("", "NA"),
    colClasses = "character"
  )
}
