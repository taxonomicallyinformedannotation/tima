#' @title Create directory
#'
#' @description This function creates a directory at the specified path if it does not already exist.
#'
#' @param export Path to the directory to be created
#'
#' @return Message indicating the status of directory creation
#'
#' @export
#'
#' @examples create_dir(export = "path/to/directory_of_file")
create_dir <- function(export) {
  ## Check if the export path includes a file name
  if (grepl(pattern = ".", x = export, fixed = TRUE)) {
    ## Create the directory at the specified path if it does not exist
    ifelse(
      test = !dir.exists(paths = dirname(path = export)),
      yes = dir.create(path = dirname(path = export), recursive = TRUE),
      no = paste(dirname(path = export), "exists")
    )
  } else {
    ## Create the directory at the specified path if it does not exist
    ifelse(
      test = !dir.exists(paths = export),
      yes = dir.create(path = export, recursive = TRUE),
      no = paste(export, "exists")
    )
  }
}
