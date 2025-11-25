#' @title Download file from URL
#'
#' @description Downloads a file from a URL with robust error handling,
#'     retry logic, and validation. Automatically creates necessary directories
#'     and validates downloaded content. Skips download if file already exists.
#'
#' @include create_dir.R
#' @include validators.R
#' @include logging_helpers.R
#' @include constants.R
#'
#' @param url Character string URL of the file to download
#' @param export Character string file path where the file should be saved
#' @param limit Integer timeout limit in seconds (default: 3600 = 1 hour)
#'
#' @return Path to the downloaded file (invisibly)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_file(
#'   url = "https://example.com/data.tsv",
#'   export = "data/source/data.tsv"
#' )
#' }
get_file <- function(url, export, limit = 3600L) {
  # Input Validation ----
  validate_character(url, param_name = "url", allow_empty = FALSE)
  validate_character(export, param_name = "export", allow_empty = FALSE)
  assert_positive_integer(limit, "limit")

  # Check if file already exists
  if (file.exists(export)) {
    logger::log_debug(
      "File already exists, skipping download: {basename(export)}"
    )
    return(invisible(export))
  }

  # Prepare for Download ----
  create_dir(export = export)
  options(timeout = limit)
  logger::log_debug("Downloading from: {url}")

  # Download File ----
  start_time <- Sys.time()
  resp <- download_with_error_handling(url, export)
  validate_http_response(resp, url, export)

  # Validate Downloaded File ----
  validate_downloaded_file(export, url)

  # Log Success ----
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  file_size <- file.info(export)$size
  log_file_op("Downloaded", export, size_bytes = file_size)
  logger::log_debug("Download completed in {format_time(elapsed)}")

  invisible(export)
}

# Helper Functions ----

#' Download file with error handling
#' @keywords internal
download_with_error_handling <- function(url, export) {
  tryCatch(
    httr2::request(base_url = url) |>
      httr2::req_progress() |>
      httr2::req_perform(path = export),
    error = function(e) {
      cleanup_failed_download(export)
      stop(
        "Failed to download from ",
        url,
        ": ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

#' Validate HTTP response status
#' @keywords internal
validate_http_response <- function(resp, url, export) {
  status <- httr2::resp_status(resp)

  if (status < 200L || status >= 300L) {
    cleanup_failed_download(export)
    stop(
      "HTTP error ",
      status,
      " downloading from: ",
      url,
      call. = FALSE
    )
  }
}

#' Validate downloaded file exists and has content
#' @keywords internal
validate_downloaded_file <- function(export, url) {
  # Check file exists
  if (!file.exists(export)) {
    stop(
      "Download failed - file not created: ",
      export,
      call. = FALSE
    )
  }

  # Check file size
  file_size <- file.size(export)
  if (file_size <= 0L) {
    cleanup_failed_download(export)
    stop(
      "Download failed - empty file from: ",
      url,
      call. = FALSE
    )
  }

  # Validate file content (detect HTML error pages)
  validate_file_content(export, url)
}

#' Validate file content is not an HTML error page
#' @keywords internal
validate_file_content <- function(export, url) {
  # Read file header
  header_raw <- read_file_header(export, n_bytes = 512L)

  if (length(header_raw) == 0L) {
    return(invisible(TRUE))
  }

  # Check for valid binary formats (zip, gzip)
  if (is_valid_binary_format(header_raw)) {
    return(invisible(TRUE))
  }

  # Check for HTML error page
  if (is_html_content(header_raw)) {
    cleanup_failed_download(export)
    stop(
      "Download failed - received HTML error page from: ",
      url,
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Read file header bytes
#' @keywords internal
read_file_header <- function(file_path, n_bytes = 512L) {
  tryCatch(
    {
      con <- file(file_path, open = "rb")
      on.exit(close(con), add = TRUE)
      readBin(con, what = "raw", n = n_bytes)
    },
    error = function(e) raw()
  )
}

#' Check if header indicates valid binary format
#' @keywords internal
is_valid_binary_format <- function(header_raw) {
  if (length(header_raw) < 2L) {
    return(FALSE)
  }

  # ZIP signature: 0x50 0x4B (PK)
  is_zip <- identical(header_raw[1:2], as.raw(c(0x50, 0x4b)))

  # GZIP signature: 0x1F 0x8B
  is_gzip <- identical(header_raw[1:2], as.raw(c(0x1f, 0x8b)))

  is_zip || is_gzip
}

#' Check if content is HTML
#' @keywords internal
is_html_content <- function(header_raw) {
  header_txt <- tryCatch(
    tolower(paste0(rawToChar(header_raw, multiple = TRUE), collapse = "")),
    error = function(e) ""
  )

  header_trim <- sub("^[\t\r\n ]+", "", header_txt)

  if (!nzchar(header_trim)) {
    return(FALSE)
  }

  grepl("^<!doctype html|^<html|^<head", header_trim, perl = TRUE)
}

#' Clean up failed download
#' @keywords internal
cleanup_failed_download <- function(export) {
  if (file.exists(export)) {
    unlink(export, force = TRUE)
  }
}
