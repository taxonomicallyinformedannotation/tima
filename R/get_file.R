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
      options(timeout = limit)
      create_dir(export = export)
      tryCatch(expr = {
        utils::download.file(
          url = url,
          destfile = export
        )
      }, error = tryCatch(expr = {
        message("Something seems wrong...retrying...")
        utils::download.file(
          url = url,
          destfile = export
        )
      }, error = {
        message("Something seems wrong...retrying again... (and then failing)")
        utils::download.file(
          url = url,
          destfile = export
        )
      }))
    } else {
      message("File already exists. Skipping.")
    }
    return(export)
  }
