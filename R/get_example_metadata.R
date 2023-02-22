#' @title Get example metadata
#'
#' @description This function gets an example metadata table to work with
#'
#' @param url URL of the example metadata table file
#' @param export File path to where the example metadata table should be saved
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom utils download.file
#'
#' @examples NULL
get_example_metadata <-
  function(url = paths$urls$examples$metadata,
           export = paths$data$source$examples$metadata) {
    paths <- parse_yaml_paths()
    ## Set the timeout for download to 600 seconds
    options(timeout = 600)
    message("Timeout for download is ", getOption("timeout"), " seconds")
    
    ## Create the export directory if it does not exist
    create_dir(export = export)
    
    ## Download the file from the given URL and save it to the specified location
    message("Downloading")
    utils::download.file(
      url = url,
      destfile = export
    )
    
    return(export)
  }