#' @title Get file
#'
#' @description This function downloads files from a URL with retry logic
#'     and exponential backoff
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
  # Validate inputs
  if (missing(url) || nchar(url) == 0L) {
    stop("URL must be specified")
  }

  if (missing(export) || nchar(export) == 0L) {
    stop("Export path must be specified")
  }

  if (limit <= 0L) {
    stop("Timeout limit must be positive")
  }

  # Check if file already exists
  if (file.exists(export)) {
    logger::log_info("File already exists, skipping download: ", export)
    return(export)
  }

  # Set timeout option
  options(timeout = limit)

  # Create output directory if needed
  create_dir(export = export)

  # Download with retry and exponential backoff
  download_with_retry <- function(url, destfile, max_attempts = 3L) {
    for (attempt in seq_len(max_attempts)) {
      tryCatch(
        {
          logger::log_debug("Download attempt ", attempt, " of ", max_attempts)

          httr2::request(url) |>
            httr2::req_progress() |>
            httr2::req_perform(path = destfile)

          if (file.exists(destfile) && file.size(destfile) > 0L) {
            logger::log_info("Successfully downloaded: ", destfile)
            return(TRUE)
          }
        },
        error = function(e) {
          logger::log_warn(
            "Download attempt ",
            attempt,
            " failed: ",
            conditionMessage(e)
          )

          # Wait before retry with exponential backoff
          if (attempt < max_attempts) {
            wait_time <- 2L^attempt
            logger::log_debug("Waiting ", wait_time, " seconds before retry...")
            Sys.sleep(wait_time)
          }
        }
      )
    }
    return(FALSE)
  }

  # Attempt download
  if (!download_with_retry(url = url, destfile = export)) {
    # Clean up partial download
    if (file.exists(export)) {
      file.remove(export)
    }
    stop("Failed to download file after multiple attempts: ", url)
  }

  return(export)
}
