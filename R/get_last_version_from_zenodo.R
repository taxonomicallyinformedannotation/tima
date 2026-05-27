#' Validate Zenodo Function Inputs
#'
#' @description Internal helper to validate all input parameters for Zenodo
#'     operations.
#'
#' @param doi Character DOI string
#' @param pattern Character pattern string
#' @param path Character path string
#' @param timeout_s Numeric timeout in seconds for Zenodo metadata requests
#'
#' @return NULL (stops on validation error)
#' @keywords internal
validate_zenodo_inputs <- function(doi, pattern, path, timeout_s) {
  if (
    missing(doi) || !is.character(doi) || length(doi) != 1L || nchar(doi) == 0L
  ) {
    cli::cli_abort(
      "doi must be a single non-empty character string",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (
    missing(pattern) ||
      !is.character(pattern) ||
      length(pattern) != 1L ||
      nchar(pattern) == 0L
  ) {
    cli::cli_abort(
      "pattern must be a single non-empty character string",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (
    missing(path) ||
      !is.character(path) ||
      length(path) != 1L ||
      nchar(path) == 0L
  ) {
    cli::cli_abort(
      "path must be a single non-empty character string",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Validate DOI format
  if (!grepl("^10\\.[0-9]+/zenodo\\.[0-9]+$", doi, perl = TRUE)) {
    cli::cli_abort(
      c(
        "invalid Zenodo DOI format",
        "x" = doi,
        "i" = "expected format: 10.5281/zenodo.XXXXXX"
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (
    missing(timeout_s) ||
      !is.numeric(timeout_s) ||
      length(timeout_s) != 1L ||
      is.na(timeout_s) ||
      timeout_s <= 0
  ) {
    cli::cli_abort(
      "timeout_s must be a single positive numeric value",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
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
#' @param timeout_s Numeric timeout in seconds for Zenodo metadata requests
#'
#' @return httr2 response object
#' @keywords internal
fetch_zenodo_record <- function(record, doi, timeout_s) {
  base_url <- "https://zenodo.org/api/records/"
  url <- paste0(base_url, record, "/versions/latest")

  log_debug("Fetching metadata from: %s", url)

  max_attempts <- as.integer(getOption("tima.zenodo.max_attempts", 5L))
  if (is.na(max_attempts) || max_attempts < 1L) {
    max_attempts <- 1L
  }
  req_retry_max_tries <- as.integer(getOption(
    "tima.zenodo.req_retry_max_tries",
    2L
  ))
  if (is.na(req_retry_max_tries) || req_retry_max_tries < 1L) {
    req_retry_max_tries <- 1L
  }
  backoff_cap_s <- as.numeric(getOption("tima.zenodo.backoff_cap_s", 8))
  if (is.na(backoff_cap_s) || backoff_cap_s < 0) {
    backoff_cap_s <- 0
  }
  last_error <- NULL

  fetch_attempt <- function(attempt) {
    response <- tryCatch(
      {
        httr2::request(base_url = url) |>
          httr2::req_timeout(seconds = timeout_s) |>
          httr2::req_retry(max_tries = req_retry_max_tries) |>
          httr2::req_perform()
      },
      httr2_http_404 = function(e) {
        cli::cli_abort(
          c(
            "Zenodo record not found",
            "x" = doi,
            "i" = conditionMessage(e)
          ),
          class = c("tima_validation_error", "tima_error"),
          call = NULL
        )
      },
      httr2_http = function(e) {
        last_error <<- e
        NULL
      },
      error = function(e) {
        last_error <<- e
        NULL
      }
    )

    if (!is.null(response)) {
      return(response)
    }

    if (attempt < max_attempts) {
      Sys.sleep(min(2^(attempt - 1L), backoff_cap_s))
      log_debug(
        "Retrying Zenodo metadata fetch (attempt %d/%d)",
        attempt + 1L,
        max_attempts
      )
      return(fetch_attempt(attempt + 1L))
    }

    cli::cli_abort(
      c(
        "failed to retrieve Zenodo record",
        "i" = "Failed to retrieve Zenodo record after retries",
        "x" = conditionMessage(last_error)
      ),
      class = c("tima_runtime_error", "tima_error"),
      call = NULL
    )
  }

  fetch_attempt(1L)
}

#' Parse Zenodo API Response
#'
#' @description Internal helper to parse JSON content from Zenodo API.

#' Find Matching File in Zenodo Record
#'
#' @description Internal helper to find a file matching the pattern in Zenodo
#'     record.
#'
#' @param filenames Character vector of filenames
#' @param pattern Character pattern to match
#' @param doi Character DOI (for error messages)
#'
#' @return Integer index of first matching file
#' @keywords internal
find_matching_file <- function(filenames, pattern, doi) {
  if (length(filenames) == 0L) {
    cli::cli_abort(
      c(
        "no files found in Zenodo record",
        "x" = doi,
        "i" = "the record may be empty or inaccessible"
      ),
      class = c("tima_runtime_error", "tima_error"),
      call = NULL
    )
  }

  indices <- grepl(pattern = pattern, x = filenames, fixed = TRUE)

  if (!any(indices)) {
    cli::cli_abort(
      c(
        "no files matching pattern found in Zenodo record",
        "x" = pattern,
        "i" = paste("available files:", paste(filenames, collapse = ", "))
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  which(indices)[1L] # Return first match index
}

#' Check if File Download is Needed
#'
#' @description Internal helper to determine if download is required by
#'     comparing sizes.
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
      "File size mismatch (local: %s, remote: %s), download needed",
      local_size,
      zenodo_size
    )
    return(TRUE)
  }

  log_debug("File exists with correct size, skipping download")
  FALSE
}

#' @title Get Latest Version from Zenodo
#'
#' @description Retrieves the latest version of a file from a Zenodo repository
#'     record.
#' This function checks the file size and only downloads if the local file is
#'     missing
#' or differs from the remote version. Implements robust error handling and
#'     retry logic.
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
#' @param timeout_s Numeric. Metadata request timeout in seconds (default: 90)
#'
#' @return Character path to the downloaded (or existing) file
#'
#' @family data-retrieval
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
get_last_version_from_zenodo <- function(doi, pattern, path, timeout_s = 90) {
  # Input Validation ----
  validate_zenodo_inputs(
    doi = doi,
    pattern = pattern,
    path = path,
    timeout_s = timeout_s
  )

  log_info("Retrieving latest version from Zenodo: %s", doi)

  # Extract Record ID ----
  record <- extract_zenodo_record_id(doi)
  log_debug("Record ID: %s", record)

  # Fetch Record Metadata ----
  record_response <- fetch_zenodo_record(
    record = record,
    doi = doi,
    timeout_s = timeout_s
  )

  # Parse API Response ----
  content <- tryCatch(
    httr2::resp_body_json(record_response),
    error = function(e) {
      cli::cli_abort(
        c(
          "failed to parse Zenodo API response",
          "x" = conditionMessage(e),
          "i" = "the API format may have changed"
        ),
        class = c("tima_runtime_error", "tima_error"),
        call = NULL
      )
    }
  )

  # Find Matching File ----
  files <- content$files
  resolved_record <- if (!is.null(content$recid) && nzchar(content$recid)) {
    content$recid
  } else if (!is.null(content$id) && nzchar(as.character(content$id))) {
    as.character(content$id)
  } else {
    record
  }
  if (is.null(files) || length(files) == 0L) {
    filenames <- character(0)
    file_urls <- character(0)
    file_sizes <- numeric(0)
  } else if (is.data.frame(files)) {
    filenames <- files$key
    if ("links.download" %in% names(files)) {
      file_urls <- files$links.download
    } else if ("links.self" %in% names(files)) {
      file_urls <- files$links.self
    } else {
      file_urls <- rep(NA_character_, nrow(files))
    }
    file_sizes <- files$size
  } else if (is.list(files) && !is.null(files$key)) {
    filenames <- files$key
    file_urls <- files$links$download %||% files$links$self
    file_sizes <- files$size
  } else {
    filenames <- vapply(
      X = files,
      FUN = function(x) {
        if (is.list(x) && !is.null(x$key)) x$key else NA_character_
      },
      FUN.VALUE = character(1)
    )
    file_urls <- vapply(
      X = files,
      FUN = function(x) {
        if (is.list(x) && is.list(x$links) && !is.null(x$links$download)) {
          x$links$download
        } else if (is.list(x) && is.list(x$links) && !is.null(x$links$self)) {
          x$links$self
        } else {
          NA_character_
        }
      },
      FUN.VALUE = character(1)
    )
    file_sizes <- vapply(
      X = files,
      FUN = function(x) {
        if (is.list(x) && !is.null(x$size)) as.numeric(x$size) else NA_real_
      },
      FUN.VALUE = numeric(1)
    )
  }

  match_idx <- find_matching_file(
    filenames = filenames,
    pattern = pattern,
    doi = doi
  )

  filename <- filenames[match_idx]
  file_url <- file_urls[match_idx]
  if (is.null(file_url) || is.na(file_url) || !nzchar(file_url)) {
    file_url <- paste0(
      "https://zenodo.org/api/records/",
      resolved_record,
      "/files/",
      filename,
      "/content"
    )
  }
  zenodo_size <- file_sizes[match_idx]

  log_debug("Found file: %s (%.0f bytes)", filename, zenodo_size)

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
      "File size: %s bytes (%.2f MB)",
      zenodo_size,
      zenodo_size / 1024^2
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

  invisible(path)
}
