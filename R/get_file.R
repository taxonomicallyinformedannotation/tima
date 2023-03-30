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
#' @examples get_file(url = "https://github.com/taxonomicallyinformedannotation/tima-example-files/raw/main/example_metadata.tsv", export = "data/source/example_metadata.tsv")
get_file <-
  function(url,
           export,
           limit = 3600) {
    if (!file.exists(export)) {
      ## Set the timeout for download
      options(timeout = limit)
      message("Timeout for download is ",
              getOption("timeout") / 60,
              " minutes")

      ## Create the export directory if it does not exist
      create_dir(export = export)

      ## Download the file from the given URL and save it to the specified location
      message("Downloading")
      utils::download.file(url = url,
                           destfile = export)
    } else {
      message("File already exists. Skipping.")
    }

    return(export)
  }
