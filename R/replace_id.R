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
#'
#' @param x Character string containing the file path with default ID
#' @param user_filename Character string for custom filename pattern
#'     (used if user_gnps is NULL)
#' @param user_gnps Character string for GNPS job ID (takes precedence)
#' @param example_gnps Character string for example GNPS job ID to detect
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
  # Input Validation ----
  validate_character(x, param_name = "x", allow_empty = FALSE)

  # Normalize Parameters ----
  # Convert empty strings to NULL for consistent handling
  user_gnps <- normalize_to_null(user_gnps)

  # Normalize example GNPS ID
  if (!is.null(user_gnps) && user_gnps == example_gnps) {
    user_gnps <- "example"
  }

  # Extract Path Components ----
  dir_path <- dirname(x)
  file_name <- basename(x)

  # Extract ID (before first underscore)
  old_id <- extract_id_prefix(file_name)

  # Determine Replacement ID ----
  # GNPS ID takes precedence over filename pattern
  new_id <- user_gnps %||% user_filename

  # Replace ID ----
  new_file_name <- replace_id_in_filename(file_name, old_id, new_id)

  # Reconstruct Path ----
  reconstruct_path(dir_path, new_file_name)
}

# Helper Functions ----

#' Normalize empty values to NULL
#' @keywords internal
normalize_to_null <- function(value) {
  if (is.null(value) || length(value) == 0L || identical(value, "")) {
    NULL
  } else {
    value
  }
}

#' Extract ID prefix from filename
#' @keywords internal
extract_id_prefix <- function(filename) {
  sub("^([^_]+)_.*$", "\\1", filename)
}

#' Replace ID in filename
#' @keywords internal
replace_id_in_filename <- function(filename, old_id, new_id) {
  sub(
    pattern = paste0("^", old_id),
    replacement = new_id,
    x = filename
  )
}

#' Reconstruct full path from directory and filename
#' @keywords internal
reconstruct_path <- function(dir_path, filename) {
  if (dir_path == ".") {
    filename
  } else {
    file.path(dir_path, filename)
  }
}

#' Null-coalescing operator (like `??` in other languages)
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
