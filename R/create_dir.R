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
  # Input Validation ----

  if (missing(export) || is.null(export) || !nzchar(export)) {
    stop("Export path must be specified and non-empty")
  }

  if (!is.character(export) || length(export) != 1L) {
    stop("Export path must be a single character string, got: ", class(export))
  }

  # Determine Directory Path ----

  # Determine if this is a file path or directory path
  # Check for file extension (contains dot in basename)
  basename_part <- basename(export)
  has_extension <- grepl("\\.[^.]+$", basename_part) && !endsWith(export, "/")

  # Extract directory path
  dirname_path <- if (has_extension) {
    dir_part <- dirname(export)
    # Normalize "." to current directory
    if (dir_part == ".") "." else dir_part
  } else {
    export
  }

  # Create Directory ----

  # Create the directory if it doesn't exist
  if (!dir.exists(dirname_path)) {
    # Create with error handling
    created <- tryCatch(
      {
        dir.create(dirname_path, recursive = TRUE, showWarnings = FALSE)
        TRUE
      },
      error = function(e) {
        logger::log_error("Failed to create directory: {dirname_path}")
        logger::log_debug("Error: {conditionMessage(e)}")
        FALSE
      }
    )

    if (created) {
      # logger::log_debug("Created directory: {dirname_path}")
    } else {
      stop("Could not create directory: ", dirname_path)
    }
  }

  # Verify Directory is Writable ----

  # Verify directory is writable (important for Docker environments)
  if (dir.exists(dirname_path)) {
    # Test write permission by trying to create a temp file
    test_file <- file.path(
      dirname_path,
      paste0(".tima_write_test_", Sys.getpid())
    )

    write_ok <- tryCatch(
      {
        writeLines("test", test_file)
        unlink(test_file, force = TRUE) # More reliable cleanup
        TRUE
      },
      error = function(e) {
        logger::log_warn(
          "Directory exists but may not be writable: {dirname_path}"
        )
        logger::log_debug("Write test error: {conditionMessage(e)}")
        FALSE
      }
    )

    if (!write_ok) {
      logger::log_warn(
        "Permission issue detected in: {dirname_path}. ",
        "This may cause failures in Docker/restricted environments."
      )
    }
  } else {
    logger::log_error("Directory does not exist after creation: {dirname_path}")
  }

  invisible(NULL)
}
