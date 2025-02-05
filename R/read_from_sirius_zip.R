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
  f <- sirius_zip |>
    utils::unzip(list = TRUE) |>
    tidytable::filter(Name |>
      grepl(pattern = file)) |>
    tidytable::arrange(Name, decreasing = TRUE) |>
    tidytable::pull(Name) |>
    # because of structures and denovo sharing their name
    head(1)
  tidytable::fread(
    file = utils::unzip(sirius_zip, f),
    na.strings = c("", "NA"),
    colClasses = "character"
  )
}
