#' Validate Zenodo Function Inputs
#'
#' @description Internal helper to validate all input parameters for Zenodo operations.
#'
#' @param doi Character DOI string
#' @param pattern Character pattern string
#' @param path Character path string
#'
#' @return NULL (stops on validation error)
#' @keywords internal
validate_zenodo_inputs <- function(doi, pattern, path) {
  if (
    missing(doi) || !is.character(doi) || length(doi) != 1L || nchar(doi) == 0L
  ) {
    stop("doi must be a single non-empty character string", call. = FALSE)
  }

  if (
    missing(pattern) ||
      !is.character(pattern) ||
      length(pattern) != 1L ||
      nchar(pattern) == 0L
  ) {
    stop("pattern must be a single non-empty character string", call. = FALSE)
  }

  if (
    missing(path) ||
      !is.character(path) ||
      length(path) != 1L ||
      nchar(path) == 0L
  ) {
    stop("path must be a single non-empty character string", call. = FALSE)
  }

  # Validate DOI format
  if (!grepl("^10\\.[0-9]+/zenodo\\.[0-9]+$", doi, perl = TRUE)) {
    stop(
      "Invalid Zenodo DOI format. Expected format: '10.5281/zenodo.XXXXXX', got: '",
      doi,
      "'",
      call. = FALSE
    )
  }

  invisible(NULL)
}

#' Extract Zenodo Record ID from DOI
#'
#' @description Internal helper to extract the record number from a Zenodo DOI.
#'
#' @param doi Character DOI string
#'
#' @return Character record ID
#' @keywords internal
extract_zenodo_record_id <- function(doi) {
  stringi::stri_replace_all_fixed(
    str = doi,
    pattern = "10.5281/zenodo.",
    replacement = "",
    case_insensitive = TRUE
  )
}

#' Fetch Zenodo Record Metadata
#'
#' @description Internal helper to retrieve record metadata from Zenodo API.
#'
#' @param record Character record ID
#' @param doi Character DOI (for error messages)
#'
#' @return httr2 response object
#' @keywords internal
fetch_zenodo_record <- function(record, doi) {
  base_url <- "https://zenodo.org/records/"
  url <- paste0(base_url, record, "/latest")

  log_debug("Fetching metadata from: %s", url)

  tryCatch(
    {
      httr2::request(base_url = url) |>
        httr2::req_timeout(seconds = 30) |>
        httr2::req_retry(max_tries = 3L) |>
        httr2::req_perform()
    },
    httr2_http_404 = function(e) {
      stop(
        "Zenodo record not found: ",
        doi,
        ". Please verify the DOI is correct.",
        call. = FALSE
      )
    },
    httr2_http = function(e) {
      stop(
        "HTTP error retrieving Zenodo record: ",
        conditionMessage(e),
        call. = FALSE
      )
    },
    error = function(e) {
      stop(
        "Failed to retrieve Zenodo record: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

#' Parse Zenodo API Response
#'
#' @description Internal helper to parse JSON content from Zenodo API.
#'
#' @param api_url Character URL to Zenodo API endpoint
#'
#' @return List with parsed JSON content
#' @keywords internal
parse_zenodo_content <- function(api_url) {
  log_debug("Parsing API response from: %s", api_url)

  tryCatch(
    {
      jsonlite::fromJSON(txt = api_url)
    },
    error = function(e) {
      stop(
        "Failed to parse Zenodo API response. ",
        "The API format may have changed. Error: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

#' Find Matching File in Zenodo Record
#'
#' @description Internal helper to find a file matching the pattern in Zenodo record.
#'
#' @param filenames Character vector of filenames
#' @param pattern Character pattern to match
#' @param doi Character DOI (for error messages)
#'
#' @return Integer index of first matching file
#' @keywords internal
find_matching_file <- function(filenames, pattern, doi) {
  if (length(filenames) == 0L) {
    stop(
      "No files found in Zenodo record: ",
      doi,
      ". The record may be empty or inaccessible.",
      call. = FALSE
    )
  }

  indices <- grepl(pattern = pattern, x = filenames, fixed = TRUE)

  if (!any(indices)) {
    stop(
      "No files matching pattern '",
      pattern,
      "' found in record ",
      doi,
      ". Available files: ",
      paste(filenames, collapse = ", "),
      call. = FALSE
    )
  }

  which(indices)[1L] # Return first match index
}

#' Check if File Download is Needed
#'
#' @description Internal helper to determine if download is required by comparing sizes.
#'
#' @param path Character local file path
#' @param zenodo_size Numeric size in bytes from Zenodo
#'
#' @return Logical TRUE if download needed, FALSE otherwise
#' @keywords internal
is_download_needed <- function(path, zenodo_size) {
  if (!file.exists(path)) {
    log_debug("File does not exist locally, download needed")
    return(TRUE)
  }

  local_size <- file.size(path)

  if (is.na(local_size)) {
    log_debug("Cannot determine local file size, download needed")
    return(TRUE)
  }

  if (local_size != zenodo_size) {
    log_debug(
      "File size mismatch (local: {local_size}, remote: {zenodo_size}), download needed"
    )
    return(TRUE)
  }

  log_debug("File exists with correct size, skipping download")
  return(FALSE)
}

#' @title Get Latest Version from Zenodo
#'
#' @description Retrieves the latest version of a file from a Zenodo repository record.
#'     This function checks the file size and only downloads if the local file is missing
#'     or differs from the remote version. Implements robust error handling and retry logic.
#'
#' @details
#' Credit goes partially to https://inbo.github.io/inborutils/
#'
#' This function:
#' \itemize{
#'   \item Validates DOI format and input parameters
#'   \item Fetches the latest version metadata from Zenodo API
#'   \item Finds files matching the specified pattern
#'   \item Compares local and remote file sizes to avoid unnecessary downloads
#'   \item Downloads only if needed, with retry logic
#'   \item Creates necessary directories automatically
#' }
#'
#' @include create_dir.R
#' @include get_file.R
#'
#' @param doi Character. Zenodo DOI (e.g., "10.5281/zenodo.5794106")
#' @param pattern Character. Pattern to identify the specific file to download
#' @param path Character. Local path where the file should be saved
#'
#' @return Character path to the downloaded (or existing) file
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Download LOTUS database from Zenodo
#' get_last_version_from_zenodo(
#'   doi = "10.5281/zenodo.5794106",
#'   pattern = "lotus.csv.gz",
#'   path = "data/source/libraries/sop/lotus.csv.gz"
#' )
#'
#' # The function will skip download if file exists with correct size
#' get_last_version_from_zenodo(
#'   doi = "10.5281/zenodo.5794106",
#'   pattern = "lotus.csv.gz",
#'   path = "data/source/libraries/sop/lotus.csv.gz"
#' )
#' }
get_last_version_from_zenodo <- function(doi, pattern, path) {
  # Input Validation ----
  validate_zenodo_inputs(doi = doi, pattern = pattern, path = path)

  log_info("Retrieving latest version from Zenodo: %s", doi)

  # Extract Record ID ----
  record <- extract_zenodo_record_id(doi)
  log_debug("Record ID: %s", record)

  # Fetch Record Metadata ----
  record_response <- fetch_zenodo_record(record = record, doi = doi)

  # Construct API URL ----
  base_url_api <- "https://zenodo.org/api/records/"
  record_id <- gsub(
    pattern = ".*/",
    replacement = "",
    x = record_response$url,
    perl = TRUE
  )
  api_url <- paste0(base_url_api, record_id)

  # Parse API Response ----
  content <- parse_zenodo_content(api_url)

  # Find Matching File ----
  filenames <- content$files$key
  match_idx <- find_matching_file(
    filenames = filenames,
    pattern = pattern,
    doi = doi
  )

  filename <- filenames[match_idx]
  file_url <- paste0(record_response$url, "/files/", filename)
  zenodo_size <- content$files$size[match_idx]

  log_debug("Found file: %s (%f2 bytes)", filename, zenodo_size)

  # Download if Needed ----
  if (is_download_needed(path = path, zenodo_size = zenodo_size)) {
    # Remove existing file if present (may be corrupted or wrong size)
    if (file.exists(path)) {
      log_debug("Removing existing file: %s", path)
      file.remove(path)
    }

    log_info(
      "Downloading %s from https://doi.org/%s",
      filename,
      doi
    )

    if (!is.null(content$metadata$title)) {
      log_debug("Record title: %s", content$metadata$title)
    }

    log_debug(
      "File size: {zenodo_size} bytes ({round(zenodo_size / 1024^2, 2)} MB)"
    )

    # Create directory and download
    create_dir(export = path)
    get_file(url = file_url, export = path)

    log_success("Download completed: %s", path)
  } else {
    log_info(
      "File already present with correct size. Skipping download."
    )
  }

  return(invisible(path))
}
