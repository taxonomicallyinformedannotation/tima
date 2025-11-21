#' @title Read from SIRIUS zip
#'
#' @description This function reads specific files from a compressed SIRIUS
#'     workspace directory. It handles file selection, filtering empty files,
#'     and parsing tab-delimited data.
#'
#' @param sirius_zip Character string path to compressed SIRIUS workspace (.zip)
#' @param file Character string pattern to match files within the archive
#'
#' @return Data frame (tidytable) containing the parsed file contents
#'
#' @examples NULL
read_from_sirius_zip <- function(sirius_zip, file) {
  # Validate inputs
  if (
    missing(sirius_zip) || !is.character(sirius_zip) || length(sirius_zip) != 1L
  ) {
    stop("sirius_zip must be a single character string")
  }

  if (!file.exists(sirius_zip)) {
    stop("SIRIUS zip file not found: ", sirius_zip)
  }

  if (missing(file) || !is.character(file) || length(file) != 1L) {
    stop("file pattern must be a single character string")
  }

  # logger::log_trace("Reading '", file, "' from SIRIUS archive: ", sirius_zip)

  # List archive contents and filter
  matching_file <- sirius_zip |>
    utils::unzip(list = TRUE) |>
    tidytable::filter(
      Name |>
        grepl(pattern = file)
    ) |>
    tidytable::arrange(Name, decreasing = TRUE) |>
    # Avoid empty files (those starting with underscore)
    tidytable::filter(
      !Name |>
        startsWith("_")
    ) |>
    tidytable::pull(Name) |>
    # Take first match (handles structures and denovo sharing names)
    utils::head(n = 1L)

  if (length(matching_file) == 0L) {
    stop(
      "No matching file found for pattern '",
      file,
      "' in archive: ",
      sirius_zip
    )
  }

  logger::log_debug("Extracting file: {matching_file}")

  # Read and parse the file from archive
  result <- archive::archive_read(archive = sirius_zip, file = matching_file) |>
    utils::read.delim(
      quote = "",
      na.strings = c("", "NA"),
      colClasses = "character",
      stringsAsFactors = FALSE
    ) |>
    tidytable::tidytable()

  # logger::log_trace("Successfully read ", nrow(result), " rows from archive")

  return(result)
}
