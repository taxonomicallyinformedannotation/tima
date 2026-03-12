#' @title Copy backbone
#'
#' @description This function copies the package backbone (default directory
#'     structure, configuration files, and parameters) to a cache directory.
#'     This sets up the working environment for TIMA workflows.
#'
#' @param cache_dir Character string path to the cache directory
#'     (default: "~/.tima" in user's home directory)
#' @param package Character string name of the package (default: "tima")
#' @param force_overwrite [logical] if TRUE, overwrite existing cache contents
#'     with package defaults; if FALSE, keep existing cache as-is
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
  cache_dir = fs::path_home(".tima"),
  package = "tima",
  force_overwrite = FALSE
) {
  # Validate inputs
  if (
    is.null(cache_dir) ||
      !is.character(cache_dir) ||
      length(cache_dir) != 1L ||
      nchar(cache_dir) == 0L
  ) {
    stop("Cache directory path must be a non-empty character string")
  }

  if (
    is.null(package) ||
      !is.character(package) ||
      length(package) != 1L ||
      nchar(package) == 0L
  ) {
    stop("Package name must be a non-empty character string")
  }

  if (!is.logical(force_overwrite) || length(force_overwrite) != 1L) {
    stop("force_overwrite must be a single logical value")
  }

  # Get package installation path
  pkg_path <- system.file(package = package)

  if (nchar(pkg_path) == 0L) {
    stop("Package '", package, "' not found. Is it installed?")
  }

  log_info("Setting up TIMA cache directory at: %s", cache_dir)

  # Create cache directory
  tryCatch(
    {
      fs::dir_create(path = cache_dir)
    },
    error = function(e) {
      log_error("Failed to create cache directory: %s", e$message)
      stop("Failed to create cache directory: ", conditionMessage(e))
    }
  )

  # Preserve existing cache by default so saved GUI parameters survive restarts
  cache_has_content <- length(list.files(cache_dir, all.files = TRUE)) > 2L
  if (cache_has_content && !force_overwrite) {
    log_info(
      "Using existing TIMA cache at %s (set force_overwrite=TRUE to refresh)",
      cache_dir
    )
    return(invisible(NULL))
  }

  # Copy package structure to cache
  tryCatch(
    {
      fs::dir_copy(
        path = pkg_path,
        new_path = cache_dir,
        overwrite = TRUE
      )
      log_info("Successfully copied package backbone to cache")
    },
    error = function(e) {
      log_error("Failed to copy package backbone: %s", e$message)
      stop("Failed to copy package backbone: ", conditionMessage(e))
    }
  )

  invisible(NULL)
}
