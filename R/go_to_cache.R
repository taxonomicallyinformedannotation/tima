#' @title Go to Cache Directory
#'
#' @description Creates a cache directory in the user's home directory (if it
#'     doesn't already exist) and changes the working directory to it. This is
#'     useful for storing temporary files, intermediate results, and downloaded
#'     data in a consistent location across sessions.
#'
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Validates the cache directory name
#'   \item Constructs the full path in the user's home directory
#'   \item Creates the directory if it doesn't exist
#'   \item Changes the working directory to the cache location
#'   \item Logs all operations for debugging
#' }
#'
#' The cache directory is created with appropriate permissions and will persist
#' across R sessions until explicitly deleted.
#'
#' @param dir Character string name of the cache directory (default: ".tima").
#'     Will be created in the user's home directory. Must be non-empty.
#'
#' @return Character string path to the cache directory (invisibly).
#'     Changes working directory as side effect.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create and navigate to default cache directory (~/.tima)
#' go_to_cache()
#'
#' # Use custom cache directory name
#' go_to_cache(dir = ".my_project_cache")
#'
#' # Store the cache path for reference
#' cache_path <- go_to_cache()
#' print(cache_path)
#' }
go_to_cache <- function(dir = ".tima") {
  # Input Validation ----
  if (
    is.null(dir) || !is.character(dir) || length(dir) != 1L || nchar(dir) == 0L
  ) {
    stop(
      "Cache directory name must be a non-empty character string, got: ",
      if (is.null(dir)) "NULL" else class(dir)[1],
      call. = FALSE
    )
  }

  # Construct Cache Path ----
  cache <- fs::path_home(dir)
  logger::log_debug("Resolved cache directory: {cache}")

  # Create Directory if Needed ----
  if (!dir.exists(cache)) {
    logger::log_info("Creating cache directory: {cache}")

    tryCatch(
      {
        fs::dir_create(path = cache)
        logger::log_success("Cache directory created successfully")
      },
      error = function(e) {
        logger::log_error("Failed to create cache directory: {e$message}")
        stop(
          "Failed to create cache directory at '",
          cache,
          "': ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  } else {
    logger::log_debug("Cache directory already exists")
  }

  # Change Working Directory ----
  tryCatch(
    {
      setwd(dir = cache)
      logger::log_info("Working directory changed to: {cache}")
    },
    error = function(e) {
      logger::log_error("Failed to change working directory: {e$message}")
      stop(
        "Failed to change to cache directory '",
        cache,
        "': ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  invisible(cache)
}
