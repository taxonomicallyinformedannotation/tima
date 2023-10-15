#' @title Get file
#'
#' @description This function get files
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
  function(url,
           export,
           limit = 3600) {
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
      tryCatch(
        expr = {
          httr2::request(base_url = url) |>
            httr2::req_error(
              is_error = function(resp) {
                return(FALSE)
              }
            ) |>
            httr2::req_perform()
          options(timeout = limit)
          create_dir(export = export)
          utils::download.file(url = url,
                               destfile = export)
        },
        error = function(e) {
          cat("There was an unexpected problem downloading your file.")
          return(export)
        }
      )
    } else {
      message("File already exists. Skipping.")
    }
    return(export)
  }
