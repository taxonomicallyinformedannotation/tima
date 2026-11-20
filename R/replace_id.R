#' @title Replace ID in file paths
#'
#' @description Replaces the default ID prefix in file paths with user-specified
#'     values. Handles GNPS job IDs and custom filename patterns. Useful for
#'     processing multiple datasets with consistent naming conventions.
#'
#' @details The function extracts the ID portion (everything before the first
#'     underscore) and replaces it with either a GNPS job ID or custom pattern.
#'     Example files are automatically detected and normalized.
#'
#' @include get_default_paths.R
#' @include get_params.R
#' @include validations_utils.R
#' @include validations_params.R
#'
#' @param x [character] The file path with default
#'     ID
#' @param user_filename [character] String for custom filename pattern
#'     (used if user_gnps is NULL)
#' @param user_gnps [character] String for GNPS job ID (takes
#'     precedence)
#' @param example_gnps [character] String for example GNPS job ID to
#'     detect
#'
#' @return Character string with ID replaced
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Replace with custom filename
#' replace_id("123456_features.tsv", user_filename = "MyData")
#' # Returns: "MyData_features.tsv"
#'
#' # Replace with GNPS ID
#' replace_id("path/123456_features.tsv", user_gnps = "abc123")
#' # Returns: "path/abc123_features.tsv"
#' }
replace_id <- function(
  x,
  user_filename = get_params(step = "prepare_params")$files$pattern,
  user_gnps = get_params(step = "prepare_params")$gnps$id,
  example_gnps = get_default_paths()$gnps$example
) {
  validate_character(x, param_name = "x", allow_empty = FALSE)

  # Convert empty strings to NULL for consistent handling
  if (
    is.null(user_gnps) || length(user_gnps) == 0L || identical(user_gnps, "")
  ) {
    user_gnps <- NULL
  }

  # Normalize example GNPS ID
  if (!is.null(user_gnps) && user_gnps == example_gnps) {
    user_gnps <- "example"
  }

  dir_path <- dirname(x)
  file_name <- basename(x)

  old_id <- sub("^([^_]+)_.*$", "\\1", file_name)

  # GNPS ID takes precedence over filename pattern
  new_id <- user_gnps %||% user_filename

  new_file_name <- sub(
    pattern = paste0("^", old_id),
    replacement = new_id,
    x = file_name
  )

  if (dir_path == ".") {
    new_file_name
  } else {
    file.path(dir_path, new_file_name)
  }
}
