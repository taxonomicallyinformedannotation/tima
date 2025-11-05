#' @title Go to cache
#'
#' @description This function creates a cache directory in the user's home
#'     directory (if it doesn't exist) and changes the working directory to it.
#'     Useful for storing temporary files and intermediate results.
#'
#' @param dir Character string name of the cache directory (default: ".tima").
#'     Will be created in the user's home directory.
#'
#' @return NULL (invisibly). Changes working directory as side effect.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create and navigate to cache directory
#' go_to_cache()
#'
#' # Use custom cache directory name
#' go_to_cache(dir = ".my_cache")
#' }
go_to_cache <- function(dir = ".tima") {
  # Validate input
  if (
    is.null(dir) || !is.character(dir) || length(dir) != 1L || nchar(dir) == 0L
  ) {
    stop("Cache directory name must be a non-empty character string")
  }

  # Construct full path to cache directory in home
  cache <- fs::path_home(dir)

  logger::log_debug("Cache directory: {cache}")

  # Create cache directory if it doesn't exist
  if (!dir.exists(cache)) {
    tryCatch(
      {
        fs::dir_create(cache)
      },
      error = function(e) {
        logger::log_error("Failed to create cache directory: {e$message}")
        stop(
          "Failed to create cache directory at ",
          cache,
          ": ",
          conditionMessage(e)
        )
      }
    )
  }

  # Change to cache directory
  tryCatch(
    {
      setwd(dir = cache)
      logger::log_info("Working directory changed to: {cache}")
    },
    error = function(e) {
      logger::log_error("Failed to change directory: {e$message}")
      stop(
        "Failed to change to cache directory ",
        cache,
        ": ",
        conditionMessage(e)
      )
    }
  )

  invisible(NULL)
}
