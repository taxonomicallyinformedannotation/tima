#' @title Get file
#'
#' @description This function get files
#'
#' @param url URL of the file to be downloaded
#' @param export File path where the file should be saved
#' @param limit Timeout limit (in seconds)
#'
#' @return The path to the file
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
get_file <-
  function(url, export, limit = 3600) {
    if (!file.exists(export)) {
      options(timeout = limit)
      create_dir(export = export)

      download_with_retry <- function(url, destfile, attempts = 3L) {
        for (i in 1L:attempts) {
          tryCatch(
            {
              httr2::request(url) |>
                httr2::req_progress() |>
                httr2::req_perform(path = destfile)
              if (file.exists(destfile)) {
                return(TRUE)
              }
            },
            error = function(e) {
              logger::log_error(
                "Something went wrong...retrying... attempt ",
                i
              )
              logger::log_error(e |> paste())
            }
          )
        }
        return(FALSE)
      }
      if (!download_with_retry(url = url, destfile = export)) {
        logger::log_fatal(
          "Failed to download the file after multiple attempts."
        )
        file.remove(export)
        stop()
      }
    } else {
      logger::log_info("File already exists. Skipping.")
    }
    return(export)
  }
