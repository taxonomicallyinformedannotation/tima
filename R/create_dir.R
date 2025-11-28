#' @title Create directory
#'
#' @description This function creates a directory at the specified path
#'     if it does not already exist. Handles both directory paths and
#'     file paths (extracting the directory component). Includes validation
#'     for write permissions.
#'
#' @include validations_utils.R
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
#' # Create a directory
#' create_dir(export = "path/to/directory")
#'
#' # Extract directory from file path and create
#' create_dir(export = "path/to/file.txt")
#' }
create_dir <- function(export) {
  # Input Validation ----
  validate_character(
    value = export,
    param_name = "export",
    allow_null = FALSE,
    allow_empty = FALSE
  )

  # Path length sanity check
  if (nchar(export) > MAX_PATH_LENGTH) {
    log_warn(
      "Path length ({nchar(export)}) exceeds typical OS maximum ({MAX_PATH_LENGTH}). ",
      "This may cause issues on some systems."
    )
  }

  # Extract Directory Path ----
  dirname_path <- extract_directory_path(export)

  # Create Directory ----
  ensure_directory_exists(dirname_path)

  # Verify Write Permissions ----
  verify_directory_writable(dirname_path)

  invisible(NULL)
}

#' Extract directory path from file or directory path
#' @keywords internal
extract_directory_path <- function(path) {
  # Determine if this is a file path or directory path
  # Check for file extension (contains dot in basename)
  basename_part <- basename(path)
  has_extension <- grepl("\\.[^.]+$", basename_part) && !endsWith(path, "/")

  if (has_extension) {
    dir_part <- dirname(path)
    # Normalize "." to current directory
    if (dir_part == ".") "." else dir_part
  } else {
    path
  }
}

#' Ensure directory exists, creating it if necessary
#' @keywords internal
ensure_directory_exists <- function(dir_path) {
  if (dir.exists(dir_path)) {
    return(invisible(TRUE))
  }

  created <- tryCatch(
    {
      dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
      log_debug("Created directory: {dir_path}")
      TRUE
    },
    error = function(e) {
      log_error("Failed to create directory: {dir_path}")
      log_debug("Error: {conditionMessage(e)}")
      FALSE
    }
  )

  if (!created) {
    stop(
      "Could not create directory: ",
      dir_path,
      "\n",
      "Please check permissions and ensure parent path exists.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Verify that a directory is writable
#' @keywords internal
verify_directory_writable <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    log_warn(
      "Directory does not exist for write verification: {dir_path}"
    )
    return(invisible(FALSE))
  }

  # Test write permission by trying to create a temp file
  test_file <- file.path(
    dir_path,
    paste0(TEMP_FILE_PREFIX, Sys.getpid())
  )

  write_ok <- tryCatch(
    {
      writeLines("test", test_file)
      unlink(test_file, force = TRUE)
      TRUE
    },
    error = function(e) {
      log_warn(
        "Directory exists but may not be writable: {dir_path}. ",
        "This may cause failures in Docker/restricted environments."
      )
      log_debug("Write test error: {conditionMessage(e)}")
      FALSE
    }
  )

  invisible(write_ok)
}
