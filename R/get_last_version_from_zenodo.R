#' @title Get last version from Zenodo
#'
#' @description This function retrieves the latest version of a file from a
#'     Zenodo repository record. It checks the file size and only downloads
#'     if the local file is missing or differs from the remote version.
#'
#' @details Credit goes partially to https://inbo.github.io/inborutils/
#'     This function handles the new Zenodo API format and file structure.
#'
#' @include create_dir.R
#' @include get_file.R
#'
#' @param doi Character string DOI of the Zenodo record (e.g., "10.5281/zenodo.5794106")
#' @param pattern Character string pattern to identify the specific file to download
#' @param path Character string local path where the file should be saved
#'
#' @return Character string path to the downloaded file
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_last_version_from_zenodo(
#'   doi = "10.5281/zenodo.5794106",
#'   pattern = "frozen.csv.gz",
#'   path = "data/frozen.csv.gz"
#' )
#' }
get_last_version_from_zenodo <- function(doi, pattern, path) {
  # Validate inputs
  if (missing(doi) || !is.character(doi) || length(doi) != 1L) {
    stop("doi must be a single character string")
  }

  if (missing(pattern) || !is.character(pattern) || length(pattern) != 1L) {
    stop("pattern must be a single character string")
  }

  if (missing(path) || !is.character(path) || length(path) != 1L) {
    stop("path must be a single character string")
  }

  logger::log_info("Retrieving latest version from Zenodo: ", doi)

  # Extract record number from DOI
  record <- stringi::stri_replace_all_fixed(
    str = doi,
    pattern = "10.5281/zenodo.",
    replacement = "",
    case_insensitive = TRUE
  )

  # Construct Zenodo API URLs
  base_url <- "https://zenodo.org/records/"
  base_url_api <- "https://zenodo.org/api/records/"

  # Retrieve latest record information
  logger::log_debug("Fetching metadata from Zenodo API")
  record_new <- tryCatch(
    {
      httr2::request(base_url = paste0(base_url, record, "/latest")) |>
        httr2::req_perform()
    },
    error = function(e) {
      stop("Failed to retrieve Zenodo record: ", conditionMessage(e))
    }
  )

  # Parse JSON content
  api_url <- paste0(
    base_url_api,
    gsub(
      pattern = ".*/",
      replacement = "",
      x = record_new$url,
      perl = TRUE
    )
  )

  content <- tryCatch(
    {
      jsonlite::fromJSON(txt = api_url)
    },
    error = function(e) {
      stop("Failed to parse Zenodo API response: ", conditionMessage(e))
    }
  )

  # Extract file information
  filenames <- content$files$key

  if (length(filenames) == 0L) {
    stop("No files found in Zenodo record: ", doi)
  }

  # Find matching file
  indices <- grepl(pattern = pattern, x = filenames, fixed = TRUE)

  if (!any(indices)) {
    stop(
      "No files matching pattern '",
      pattern,
      "' found in record. ",
      "Available files: ",
      paste(filenames, collapse = ", ")
    )
  }

  filename <- filenames[indices][1L] # Take first match
  file_url <- paste0(record_new$url, "/files/", filename)

  # Check if download is needed by comparing file sizes
  zenodo_size <- content$files$size[indices][1L]
  local_size <- file.size(path)

  if (is.na(local_size) || zenodo_size != local_size) {
    logger::log_info(
      "Downloading ",
      filename,
      " from https://doi.org/",
      doi,
      " (",
      content$metadata$title,
      ")"
    )
    logger::log_debug("Size: ", zenodo_size, " bytes")

    create_dir(export = path)
    get_file(url = file_url, export = path)
  } else {
    logger::log_info(
      "File already present with correct size. Skipping download."
    )
  }

  return(path)
}
