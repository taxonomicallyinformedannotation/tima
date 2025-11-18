#' @title Get file
#'
#' @description This function downloads files from a URL with retry logic
#'     and exponential backoff.
#'
#' @param url Character string URL of the file to be downloaded
#' @param export Character string file path where the file should be saved
#' @param limit Integer timeout limit in seconds (default: 3600)
#'
#' @return The path to the downloaded file
#'
#' @export
#'
#' @examples
#' \dontrun{
#' git <- "https://github.com/"
#' org <- "taxonomicallyinformedannotation"
#' repo <- "tima-example-files"
#' branch <- "main"
#' file <- "example_metadata.tsv"
#' get_file(
#'   url = paste(git, org, repo, "raw", branch, file, sep = "/"),
#'   export = "data/source/example_metadata.tsv"
#' )
#' }
get_file <- function(url, export, limit = 3600L) {
  # ============================================================================
  # Input Validation (early checks)
  # ============================================================================

  if (missing(url) || !is.character(url) || !nzchar(url)) {
    stop("URL must be a non-empty character string")
  }

  if (missing(export) || !is.character(export) || !nzchar(export)) {
    stop("Export path must be a non-empty character string")
  }

  if (!is.numeric(limit) || limit <= 0) {
    stop("Timeout limit must be a positive number, got: ", limit)
  }

  # ============================================================================
  # Check if File Already Exists (early exit)
  # ============================================================================

  # Early exit if file already exists
  if (file.exists(export)) {
    file_size_mb <- round(file.info(export)$size / 1024^2, 2)
    logger::log_info(
      "File already exists ({file_size_mb} MB), skipping download: {export}"
    )
    return(export)
  }

  # ============================================================================
  # Setup Download
  # ============================================================================

  # Set timeout option
  options(timeout = limit)
  logger::log_info("Downloading file from: {url}")
  # logger::log_debug("Timeout limit: {limit} seconds")

  # Create output directory if needed
  create_dir(export = export)

  # ============================================================================
  # Download with Retry and Exponential Backoff
  # ============================================================================

  # Download function with retry logic
  download_with_retry <- function(url, destfile, max_attempts = 3L) {
    for (attempt in seq_len(max_attempts)) {
      success <- tryCatch(
        {
          # Perform download with progress bar
          resp <- httr2::request(url) |>
            httr2::req_progress() |>
            httr2::req_perform(path = destfile)

          # Verify HTTP status is 2xx
          status <- httr2::resp_status(resp)
          if (status < 200L || status >= 300L) {
            # Clean up and signal failure
            if (file.exists(destfile)) {
              unlink(destfile, force = TRUE)
            }
            stop(sprintf("HTTP status %s for URL: %s", status, url))
          }

          # Validate downloaded file
          if (file.exists(destfile) && file.size(destfile) > 0L) {
            file_size_mb <- round(file.info(destfile)$size / 1024^2, 2)
            logger::log_info(
              "Successfully downloaded {file_size_mb} MB to: {destfile}"
            )
            TRUE
          } else {
            logger::log_warn("Downloaded file appears to be empty or missing")
            FALSE
          }
        },
        error = function(e) {
          logger::log_warn(
            "Download attempt {attempt} failed: {conditionMessage(e)}"
          )
          # Ensure no partial file remains
          if (file.exists(destfile)) {
            unlink(destfile, force = TRUE)
          }
          # Wait before retry with exponential backoff
          if (attempt < max_attempts) {
            wait_time <- 2L^attempt
            logger::log_debug("Waiting {wait_time} seconds before retry...")
            Sys.sleep(wait_time)
          }
          FALSE
        }
      )

      # Return immediately on success
      if (success) return(TRUE)
    }
    FALSE
  }

  # Attempt download
  download_success <- download_with_retry(url = url, destfile = export)

  if (!download_success) {
    # Clean up partial download
    if (file.exists(export)) {
      # logger::log_trace("Removing incomplete download")
      unlink(export, force = TRUE)
    }
    logger::log_error("Failed to download file after multiple attempts")
    stop("Failed to download file after multiple attempts from: ", url)
  }

  # Additional validation: ensure non-empty file and not an HTML error page
  finfo <- tryCatch(file.info(export), error = function(e) NULL)
  if (is.null(finfo) || is.na(finfo$size) || finfo$size <= 0L) {
    if (file.exists(export)) {
      unlink(export, force = TRUE)
    }
    stop("Downloaded file is empty or inaccessible: ", export)
  }

  # Optional read of first bytes to detect HTML error pages (basic heuristic)
  con <- file(export, open = "rb")
  header <- tryCatch(
    rawToChar(readBin(con, what = "raw", n = 256L)),
    finally = close(con)
  )
  if (grepl("^[[:space:]]*<", header)) {
    # Looks like HTML; treat as error (likely an error page)
    unlink(export, force = TRUE)
    stop(
      "Downloaded content appears to be HTML (likely an error page) from: ",
      url
    )
  }

  return(export)
}
