#' @title Replace ID in file paths
#'
#' @description This function replaces the default ID in file paths with
#'     user-specified values. It handles both GNPS job IDs and custom
#'     filename patterns, with special handling for example files.
#'
#' @include get_default_paths.R
#' @include get_params.R
#'
#' @param x Character string containing the file path with default ID
#' @param user_filename Character string for a custom filename pattern
#' @param user_gnps Character string for a GNPS job ID (if NULL, uses user_filename)
#' @param example_gnps Character string for the example GNPS job ID to detect
#'
#' @return Character string with the ID replaced according to user specifications
#'
#' @examples
#' \dontrun{
#' replace_id(
#'   x = "example/123456_features.tsv",
#'   user_gnps = NULL,
#'   user_filename = "Foo"
#' )
#' }
replace_id <- function(
  x,
  user_filename = get_params(step = "prepare_params")$files$pattern,
  user_gnps = get_params(step = "prepare_params")$gnps$id,
  example_gnps = get_default_paths()$gnps$example
) {
  # Validate input
  if (missing(x) || is.null(x) || nchar(x) == 0L) {
    stop("File path 'x' must be specified")
  }

  # Normalize empty strings to NULL
  if (!is.null(user_gnps) && (length(user_gnps) == 0L || user_gnps == "")) {
    user_gnps <- NULL
  }

  # Replace example GNPS ID with "example" for consistency
  if (!is.null(user_gnps) && user_gnps == example_gnps) {
    user_gnps <- "example"
  }

  # Extract path and filename components
  dir_path <- dirname(x)
  file_name <- basename(x)

  # Extract the ID portion (everything before the first underscore)
  old_id <- sub("^([^_]+)_.*$", "\\1", file_name)

  # Determine new ID: use GNPS ID if provided, otherwise use filename pattern
  new_id <- if (!is.null(user_gnps)) user_gnps else user_filename

  # Replace old ID with new ID in filename
  new_file_name <- sub(
    pattern = paste0("^", old_id),
    replacement = new_id,
    x = file_name
  )

  # Reconstruct full path
  if (dir_path == ".") {
    # No directory component in original path
    return(new_file_name)
  } else {
    return(file.path(dir_path, new_file_name))
  }
}
