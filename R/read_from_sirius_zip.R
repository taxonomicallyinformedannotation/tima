.SIRIUS_ZIP_LIST_CACHE <- new.env(hash = TRUE, parent = emptyenv())

.get_sirius_zip_names <- function(sirius_zip) {
  info <- file.info(sirius_zip)
  cache_key <- paste(
    sirius_zip,
    as.numeric(info$mtime),
    info$size,
    sep = "|"
  )
  cached <- get0(cache_key, envir = .SIRIUS_ZIP_LIST_CACHE, inherits = FALSE)
  if (!is.null(cached)) {
    return(cached)
  }
  zip_names <- as.character(utils::unzip(sirius_zip, list = TRUE)$Name)
  if (!is.na(info$size)) {
    assign(cache_key, zip_names, envir = .SIRIUS_ZIP_LIST_CACHE)
  }
  zip_names
}

#' @title Read file from SIRIUS zip archive
#'
#' @description Reads specific files from a compressed SIRIUS workspace
#'     directory. Handles file selection, filtering empty files, and
#' parsing tab-delimited data. Internal helper for prepare_annotations_sirius().
#'
#' @include validations_utils.R
#' @include logs_utils.R
#' @include validations_params.R
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
  zip_names <- .get_sirius_zip_names(sirius_zip)
  keep_idx <- which(
    grepl(pattern = file, x = zip_names, perl = TRUE) &
      !startsWith(zip_names, "_")
  )
  if (length(keep_idx) == 0L) {
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
  matched_names <- zip_names[keep_idx]
  if (length(matched_names) > 1L) {
    matched_names <- sort(matched_names, method = "radix")
  }
  matching_file <- matched_names[[1L]]

  log_debug("Extracting file: %s", matching_file)

  # Read and parse the file from archive
  con <- unz(description = sirius_zip, filename = matching_file)
  result <- .read_sirius_tabular(con)

  result
}
