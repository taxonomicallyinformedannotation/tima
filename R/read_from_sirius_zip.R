#' @title Read file from SIRIUS zip archive
#'
#' @description Reads specific files from a compressed SIRIUS workspace
#'     directory. Handles file selection, filtering empty files, and
#'     parsing tab-delimited data. Internal helper for prepare_annotations_sirius().
#'
#' @include validations_utils.R
#'
#' @param sirius_zip [character] Path to compressed SIRIUS workspace (.zip)
#' @param file [character] Pattern to match files within the archive
#'
#' @return Data frame (tidytable) containing the parsed file contents
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Internal use only - called by prepare_annotations_sirius()
#' formulas <- read_from_sirius_zip(
#'   sirius_zip = "path/to/sirius_workspace.zip",
#'   file = "formula_candidates.tsv"
#' )
#' }
read_from_sirius_zip <- function(sirius_zip, file) {
  # Input Validation ----
  validate_character(
    sirius_zip,
    param_name = "sirius_zip",
    allow_empty = FALSE
  )

  validate_character(file, param_name = "file", allow_empty = FALSE)

  # Validate file exists
  if (!file.exists(sirius_zip)) {
    cli::cli_abort(
      c(
        "SIRIUS zip file not found",
        "x" = sirius_zip
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # List archive contents and filter
  matching_file <- sirius_zip |>
    utils::unzip(list = TRUE) |>
    tidytable::filter(
      Name |>
        grepl(pattern = file, perl = TRUE)
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
    cli::cli_abort(
      c(
        "No matching file found in archive",
        "x" = paste0("pattern=", file),
        "i" = sirius_zip
      ),
      class = c("tima_runtime_error", "tima_error"),
      call = NULL
    )
  }

  log_debug("Extracting file: %s", matching_file)

  # Read and parse the file from archive
  result <- archive::archive_read(archive = sirius_zip, file = matching_file) |>
    utils::read.delim(
      quote = "",
      na.strings = c("", "NA"),
      colClasses = "character",
      stringsAsFactors = FALSE
    ) |>
    tidytable::tidytable()

  return(result)
}
