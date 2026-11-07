.copy_backbone_dir_create <- function(path) {
  if (
    !dir.exists(path) &&
      !dir.create(path = path, recursive = TRUE, showWarnings = FALSE)
  ) {
    stop(sprintf("Failed to create directory: %s", path), call. = FALSE)
  }
}

.copy_backbone_dir_copy <- function(path, new_path, overwrite = TRUE) {
  .copy_backbone_dir_create(new_path)
  items <- list.files(path, all.files = TRUE, full.names = TRUE, no.. = TRUE)
  if (length(items) > 0L) {
    ok <- file.copy(
      from = items,
      to = new_path,
      overwrite = overwrite,
      recursive = TRUE
    )
    if (!all(ok)) {
      stop(
        sprintf("Failed to copy directory from %s to %s", path, new_path),
        call. = FALSE
      )
    }
  }
}

#' @title Copy backbone
#'
#' @description This function copies the package backbone (default directory
#'     structure, configuration files, and parameters) to a cache directory.
#'     This sets up the working environment for TIMA workflows.
#'
#' @include logs_utils.R
#' @param cache_dir Character string path to the cache directory
#'     (default: "~/.tima" in user's home directory)
#' @param package Character string name of the package (default: "tima")
#'
#' @return NULL (invisibly). Creates cache directory structure as side effect.
#'
#' @examples
#' \dontrun{
#' # Copy to default cache location
#' copy_backbone()
#'
#' # Copy to custom location
#' copy_backbone(cache_dir = "~/my_tima_cache")
#' }
copy_backbone <- function(
  cache_dir = path.expand("~/.tima"),
  package = "tima"
) {
  if (
    is.null(cache_dir) ||
      !is.character(cache_dir) ||
      length(cache_dir) != 1L ||
      nchar(cache_dir) == 0L
  ) {
    cli::cli_abort(
      "cache directory path must be a non-empty character string",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (
    is.null(package) ||
      !is.character(package) ||
      length(package) != 1L ||
      nchar(package) == 0L
  ) {
    cli::cli_abort(
      "package name must be a non-empty character string",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Get package installation path
  pkg_path <- system.file(package = package)

  if (nchar(pkg_path) == 0L) {
    cli::cli_abort(
      c(
        "package not found. is it installed?",
        "x" = package
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  log_info("Setting up TIMA cache directory at: %s", cache_dir)

  # Create cache directory
  tryCatch(
    {
      .copy_backbone_dir_create(path = cache_dir)
    },
    error = function(e) {
      log_error("Failed to create cache directory: %s", e$message)
      cli::cli_abort(
        c(
          "failed to create cache directory",
          "x" = conditionMessage(e)
        ),
        class = c("tima_runtime_error", "tima_error"),
        call = NULL
      )
    }
  )

  # Copy package structure to cache
  tryCatch(
    {
      .copy_backbone_dir_copy(
        path = pkg_path,
        new_path = cache_dir,
        overwrite = TRUE
      )
      log_info("Successfully copied package backbone to cache")
    },
    error = function(e) {
      log_error("Failed to copy package backbone: %s", e$message)
      cli::cli_abort(
        c(
          "failed to copy package backbone",
          "x" = conditionMessage(e)
        ),
        class = c("tima_runtime_error", "tima_error"),
        call = NULL
      )
    }
  )

  invisible(NULL)
}
