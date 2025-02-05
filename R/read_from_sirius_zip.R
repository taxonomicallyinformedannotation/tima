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
    # avoid empty files
    tidytable::filter(!Name |> startsWith("_")) |>
    tidytable::pull(Name) |>
    # because of structures and denovo sharing their name
    head(1)
  archive::archive_read(sirius_zip, f) |>
    read.delim(
      na.strings = c("", "NA"),
      colClasses = "character",
      stringsAsFactors = FALSE
    ) |>
    tidytable::tidytable()
}
