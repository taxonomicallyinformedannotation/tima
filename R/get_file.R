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
  if (missing(url)) {
    stop("URL must be provided")
  }
  if (missing(export)) {
    stop("Export path must be provided")
  }

  if (!is.character(url)) {
    stop("URL must be a non-empty character string")
  }
  if (!is.character(export)) {
    stop("Export path must be a non-empty character string")
  }
  if (length(url) != 1L) {
    stop("URL must be a non-empty character string")
  }
  if (length(export) != 1L) {
    stop("Export path must be a non-empty character string")
  }
  if (!nzchar(url)) {
    stop("URL must be a non-empty character string")
  }
  if (!nzchar(export)) {
    stop("Export path must be a non-empty character string")
  }

  if (!is.numeric(limit)) {
    stop("Timeout limit must be a positive number")
  }
  if (length(limit) != 1L) {
    stop("Timeout limit must be a positive number")
  }
  if (limit <= 0) {
    stop("Timeout limit must be a positive number")
  }

  if (file.exists(export)) {
    logger::log_info("File already exists, skipping download: {export}")
    return(invisible(export))
  }

  create_dir(export = export)
  options(timeout = limit)
  logger::log_info("Downloading file from: {url}")

  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_progress() |>
      httr2::req_perform(path = export),
    error = function(e) {
      if (file.exists(export)) {
        unlink(export, force = TRUE)
      }
      stop(paste0("Failed to download (connection error) from: ", url))
    }
  )

  status <- httr2::resp_status(resp)
  if (status < 200L) {
    if (file.exists(export)) {
      unlink(export, force = TRUE)
    }
    stop(paste0("Failed to download (HTTP ", status, ") from: ", url))
  }
  if (status >= 300L) {
    if (file.exists(export)) {
      unlink(export, force = TRUE)
    }
    stop(paste0("Failed to download (HTTP ", status, ") from: ", url))
  }

  file_exists_check <- file.exists(export)
  if (!file_exists_check) {
    stop(paste0("Failed to download (file not created) from: ", url))
  }

  file_size_check <- file.size(export)
  if (file_size_check <= 0L) {
    unlink(export, force = TRUE)
    stop(paste0("Failed to download (empty file) from: ", url))
  }

  header_raw <- tryCatch(
    {
      con <- file(export, open = "rb")
      on.exit(close(con), add = TRUE)
      readBin(con, what = "raw", n = 512L)
    },
    error = function(e) raw()
  )

  is_zip <- FALSE
  if (length(header_raw) >= 2L) {
    if (identical(header_raw[1:2], as.raw(c(0x50, 0x4b)))) {
      is_zip <- TRUE
    }
  }

  is_gzip <- FALSE
  if (length(header_raw) >= 2L) {
    if (identical(header_raw[1:2], as.raw(c(0x1f, 0x8b)))) {
      is_gzip <- TRUE
    }
  }

  if (!is_zip) {
    if (!is_gzip) {
      if (length(header_raw) > 0L) {
        header_txt <- tryCatch(
          tolower(paste0(
            rawToChar(header_raw, multiple = TRUE),
            collapse = ""
          )),
          error = function(e) ""
        )
        header_trim <- sub("^[\t\r\n ]+", "", header_txt)
        if (nzchar(header_trim)) {
          if (
            grepl("^<!doctype html|^<html|^<head", header_trim, perl = TRUE)
          ) {
            unlink(export, force = TRUE)
            stop(paste0("Failed to download (HTML error page) from: ", url))
          }
        }
      }
    }
  }

  file_size_mb <- round(file.info(export)$size / 1024^2, 2)
  logger::log_info("Successfully downloaded {file_size_mb} MB to: {export}")
  invisible(export)
}
