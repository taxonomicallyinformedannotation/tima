#' @title Create directory
#'
#' @description This function creates a directory at the specified path
#'    if it does not already exist.
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
    dirname_path <- dirname(export)
    file_name <- basename(export)
  } else {
    dirname_path <- export
    file_name <- ""
  }

  ## Create the directory at the specified path if it does not exist
  if (!dir.exists(dirname_path)) {
    dir.create(dirname_path, recursive = TRUE)
    log_debug("Directory", dirname_path, "created.")
  } else {
    log_debug("Directory", dirname_path, "already exists.")
  }

  if (file_name != "") {
    log_debug("File", file_name, "will be saved in the directory", dirname_path)
  }
}
