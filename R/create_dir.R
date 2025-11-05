#' @title Create directory
#'
#' @description This function creates a directory at the specified path
#'     if it does not already exist. Handles both directory paths and
#'     file paths (extracting the directory component).
#'
#' @param export Character string path to the directory or file path
#'     from which to extract and create the directory
#'
#' @return NULL (invisibly). Creates directory as a side effect.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_dir(export = "path/to/directory")
#' create_dir(export = "path/to/file.txt")
#' }
create_dir <- function(export) {
  # Validate input
  if (missing(export) || is.null(export) || nchar(export) == 0L) {
    stop("Export path must be specified")
  }

  if (!is.character(export) || length(export) != 1L) {
    stop("Export path must be a single character string")
  }

  # Determine if this is a file path or directory path
  # Check for file extension (contains dot in basename)
  basename_part <- basename(export)
  has_extension <- grepl("\\.[^.]+$", basename_part) && !endsWith(export, "/")

  dirname_path <- if (has_extension) {
    dir_part <- dirname(export)
    if (dir_part == ".") "." else dir_part
  } else {
    export
  }

  # Create the directory if it doesn't exist
  if (!dir.exists(dirname_path)) {
    dir.create(dirname_path, recursive = TRUE, showWarnings = FALSE)
    logger::log_debug("Created directory: {dirname_path}")
  }

  invisible(NULL)
}
