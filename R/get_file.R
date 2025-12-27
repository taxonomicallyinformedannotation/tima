#' @title Download file from URL
#'
#' @description Downloads a file from a URL with robust error handling,
#'     retry logic, and validation. Automatically creates necessary directories
#'     and validates downloaded content. Skips download if file already exists.
#'
#' @include constants.R
#' @include create_dir.R
#' @include logs_utils.R
#' @include retry_utils.R
#' @include validations_utils.R
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
    log_debug(
      "File already exists, skipping download: %s",
      basename(export)
    )
    return(invisible(export))
  }

  # Prepare for Download ----
  create_dir(export = export)
  options(timeout = limit)
  log_debug("Downloading from: %s", url)

  # Download File ----
  ctx <- log_operation("download_file", url = url, destination = export)

  resp <- download_with_error_handling(url, export)
  validate_http_response(resp, url, export)

  # Validate Downloaded File ----
  validate_downloaded_file(export, url)

  # Log Success ----
  file_size <- file.info(export)$size
  log_complete(ctx, size_bytes = file_size)

  invisible(export)
}

# Helper Functions ----

#' Download file with error handling and retry
#' @keywords internal
download_with_error_handling <- function(url, export) {
  tryCatch(
    with_retry(
      expr = {
        httr2::request(base_url = url) |>
          httr2::req_progress() |>
          httr2::req_perform(path = export)
      },
      max_attempts = 3L,
      backoff = 2,
      operation_name = "file download"
    ),
    error = function(e) {
      cleanup_failed_download(export)
      stop(e)
    }
  )
}

#' Validate HTTP response status
#' @keywords internal
validate_http_response <- function(resp, url, export) {
  status <- httr2::resp_status(resp = resp)

  if (status < 200L || status >= 300L) {
    cleanup_failed_download(export)

    # Provide helpful context based on status code
    status_explanation <- switch(
      as.character(status),
      "404" = "Resource not found - URL may be incorrect or resource removed",
      "403" = "Access forbidden - authentication or permissions required",
      "401" = "Unauthorized - credentials may be required",
      "500" = "Server error - try again later",
      "503" = "Service unavailable - server temporarily down",
      "301" = "Resource moved permanently - update URL",
      "302" = "Resource moved temporarily",
      paste0("HTTP error ", status)
    )

    msg <- format_error(
      problem = status_explanation,
      expected = "HTTP status 200-299 (success)",
      received = paste0("HTTP ", status),
      location = url,
      suggestion = if (status == 404) {
        "Double-check the URL spelling and path"
      } else if (status %in% c(500, 503)) {
        "Server is experiencing issues - try again in a few minutes"
      } else {
        NULL
      },
      fix = paste0(
        "1. Verify the URL is correct\n",
        "2. Check if resource has moved\n",
        "3. Contact data provider if issue persists"
      )
    )
    stop(msg, call. = FALSE)
  }
}

#' Validate downloaded file exists and has content
#' @keywords internal
validate_downloaded_file <- function(export, url) {
  # Check file exists
  if (!file.exists(export)) {
    msg <- format_error(
      problem = "Download completed but file not created",
      expected = paste0("File at: ", export),
      location = url,
      context = "File system permissions or disk space issues may prevent file creation",
      fix = paste0(
        "1. Check directory exists and is writable\n",
        "2. Verify sufficient disk space\n",
        "3. Check file system permissions"
      )
    )
    stop(msg, call. = FALSE)
  }

  # Check file size
  file_size <- file.size(export)
  if (file_size <= 0L) {
    cleanup_failed_download(export)
    msg <- format_error(
      problem = "Downloaded file is empty (0 bytes)",
      expected = "File with data content",
      received = "0 byte file",
      location = url,
      context = "Server may have returned empty response or download was interrupted",
      fix = paste0(
        "1. Verify the URL points to actual data\n",
        "2. Check if server requires authentication\n",
        "3. Try downloading manually to test\n",
        "4. Check server logs if you have access"
      )
    )
    stop(msg, call. = FALSE)
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
    msg <- format_error(
      problem = "Server returned HTML error page instead of data",
      expected = "Binary data file (ZIP, GZIP) or text data",
      received = "HTML error page",
      location = url,
      context = paste0(
        "Server sent an HTML error page instead of the requested file. ",
        "This often happens when:\n",
        "  - URL is incorrect or resource moved\n",
        "  - Authentication is required\n",
        "  - Server encountered an error"
      ),
      fix = paste0(
        "1. Open the URL in a web browser to see the actual error\n",
        "2. Check if authentication or API key is required\n",
        "3. Verify the URL path is correct\n",
        "4. Contact the data provider for correct download URL"
      )
    )
    stop(msg, call. = FALSE)
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
