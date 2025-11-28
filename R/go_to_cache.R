#' @title Navigate to cache directory
#'
#' @description Creates and navigates to a cache directory in the user's home
#'     directory. Useful for storing temporary files, intermediate results, and
#'     downloaded data in a consistent location across sessions.
#'
#' @details The function:
#' \itemize{
#'   \item Constructs full path in user's home directory
#'   \item Creates directory if it doesn't exist
#'   \item Changes working directory to cache location
#'   \item Logs all operations
#' }
#'
#' Cache directory persists across R sessions until explicitly deleted.
#'
#' @include validations_utils.R
#'
#' @param dir Character string name of cache directory (default: ".tima").
#'     Created in user's home directory. Must be non-empty.
#'
#' @return Path to cache directory (invisibly). Changes working directory
#'     as side effect.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Default cache (~/.tima)
#' go_to_cache()
#'
#' # Custom cache
#' go_to_cache(dir = ".my_cache")
#'
#' # Store path
#' cache_path <- go_to_cache()
#' }
go_to_cache <- function(dir = ".tima") {
  # Input Validation ----
  validate_character(dir, param_name = "dir", allow_empty = FALSE)

  # Construct and Create Cache Path ----
  cache <- fs::path_home(dir)
  log_debug("Cache directory: %s", cache)

  ensure_cache_exists(cache)

  # Change Working Directory ----
  change_to_cache(cache)

  invisible(cache)
}

# Helper Functions ----

#' Ensure cache directory exists
#' @keywords internal
ensure_cache_exists <- function(cache) {
  if (dir.exists(cache)) {
    log_debug("Cache exists")
    return(invisible(TRUE))
  }

  log_info("Creating cache: %s", cache)

  tryCatch(
    {
      fs::dir_create(path = cache)
      log_debug("Cache created")
    },
    error = function(e) {
      log_error("Failed to create cache: %s", conditionMessage(e))
      stop(
        "Cannot create cache directory '",
        cache,
        "': ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

#' Change to cache directory
#' @keywords internal
change_to_cache <- function(cache) {
  tryCatch(
    {
      setwd(dir = cache)
      log_info("Working directory: %s", cache)
    },
    error = function(e) {
      log_error("Failed to change directory: %s", conditionMessage(e))
      stop(
        "Cannot change to cache directory '",
        cache,
        "': ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
}
