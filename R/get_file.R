import::from(httr2, req_perform, .into = environment())
import::from(httr2, req_progress, .into = environment())
import::from(httr2, request, .into = environment())

#' @title Get file
#'
#' @description This function get files
#'
#' @importFrom httr2 req_perform
#' @importFrom httr2 req_progress
#' @importFrom httr2 request
#'
#' @param url URL of the file to be downloaded
#' @param export File path where the file should be saved
#' @param limit Timeout limit (in seconds)
#'
#' @return NULL
#'
#' @export
#'
#' @examples
#' git <- "https://github.com/"
#' org <- "taxonomicallyinformedannotation"
#' repo <- "tima-example-files"
#' branch <- "main"
#' file <- "example_metadata.tsv"
#' get_file(
#'   url = paste(git, org, repo, "raw", branch, file, sep = "/"),
#'   export = "data/source/example_metadata.tsv"
#' )
get_file <-
  function(url, export, limit = 3600) {
    # ## Use default system data directory
    # export <- file.path(
    #   rappdirs::user_data_dir(
    #     appname = appname,
    #     appauthor = appauthor,
    #     version = version
    #   ),
    #   export
    # )

    if (!file.exists(export)) {
      options(timeout = limit)
      create_dir(export = export)

      download_with_retry <- function(url, destfile, attempts = 3) {
        for (i in 1:attempts) {
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
              message("Something seems wrong...retrying... attempt ", i)
            }
          )
        }
        return(FALSE)
      }
      if (!download_with_retry(url = url, destfile = export)) {
        message("Failed to download the file after multiple attempts.")
      }
    } else {
      message("File already exists. Skipping.")
    }
    return(export)
  }
