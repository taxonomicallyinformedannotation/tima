#' Progress Bar Wrapper for Batch Operations
#'
#' @description Wraps purrr::map operations with a progress bar for better UX
#'     during long-running batch operations.
#'
#' @param items Vector or list to iterate over
#' @param .f Function to apply to each item
#' @param desc Character description for progress bar
#' @param show_progress Logical whether to show progress bar (default: TRUE)
#' @param ... Additional arguments passed to .f
#'
#' @return List of results from applying .f to items
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Process files with progress
#' results <- with_progress(
#'   items = file_list,
#'   .f = process_file,
#'   desc = "Processing files"
#' )
#'
#' # Classify structures with progress
#' classified <- with_progress(
#'   items = structures,
#'   .f = classify_structure,
#'   desc = "Classifying structures"
#' )
#' }
with_progress <- function(
  items,
  .f,
  desc = "Processing",
  show_progress = TRUE,
  ...
) {
  n <- length(items)

  # Skip progress bar for small batches or if disabled
  if (!show_progress || n < 10) {
    return(purrr::map(items, .f, ...))
  }

  # Check if progress package is available
  has_progress <- requireNamespace("progress", quietly = TRUE)

  if (!has_progress) {
    log_debug("progress package not available, running without progress bar")
    return(purrr::map(items, .f, ...))
  }

  # Create progress bar
  pb <- progress::progress_bar$new(
    format = paste0(
      desc,
      " [:bar] :current/:total (:percent) | Elapsed: :elapsed | ETA: :eta"
    ),
    total = n,
    clear = FALSE,
    width = 80
  )

  # Process with progress updates
  purrr::map(items, function(item) {
    result <- .f(item, ...)
    pb$tick()
    result
  })
}

#' Progress Bar for map2 Operations
#'
#' @description Wraps purrr::map2 with progress bar
#'
#' @param .x First vector to iterate over
#' @param .y Second vector to iterate over
#' @param .f Function taking two arguments
#' @param desc Character progress description
#' @param show_progress Logical whether to show progress
#' @param ... Additional arguments to .f
#'
#' @return List of results
#'
#' @keywords internal
with_progress2 <- function(
  .x,
  .y,
  .f,
  desc = "Processing",
  show_progress = TRUE,
  ...
) {
  n <- length(.x)

  if (!show_progress || n < 10) {
    return(purrr::map2(.x, .y, .f, ...))
  }

  has_progress <- requireNamespace("progress", quietly = TRUE)

  if (!has_progress) {
    return(purrr::map2(.x, .y, .f, ...))
  }

  pb <- progress::progress_bar$new(
    format = paste0(
      desc,
      " [:bar] :current/:total (:percent) | Elapsed: :elapsed | ETA: :eta"
    ),
    total = n,
    clear = FALSE,
    width = 80
  )

  purrr::map2(.x, .y, function(x, y) {
    result <- .f(x, y, ...)
    pb$tick()
    result
  })
}

#' Progress Bar for imap Operations
#'
#' @description Wraps purrr::imap with progress bar
#'
#' @param .x Vector to iterate over (with names/indices)
#' @param .f Function taking value and name/index
#' @param desc Character progress description
#' @param show_progress Logical whether to show progress
#' @param ... Additional arguments to .f
#'
#' @return List of results
#'
#' @keywords internal
with_progress_imap <- function(
  .x,
  .f,
  desc = "Processing",
  show_progress = TRUE,
  ...
) {
  n <- length(.x)

  if (!show_progress || n < 10) {
    return(purrr::imap(.x, .f, ...))
  }

  has_progress <- requireNamespace("progress", quietly = TRUE)

  if (!has_progress) {
    return(purrr::imap(.x, .f, ...))
  }

  pb <- progress::progress_bar$new(
    format = paste0(
      desc,
      " [:bar] :current/:total (:percent) | Elapsed: :elapsed | ETA: :eta"
    ),
    total = n,
    clear = FALSE,
    width = 80
  )

  purrr::imap(.x, function(value, name) {
    result <- .f(value, name, ...)
    pb$tick()
    result
  })
}

#' Simple Progress Counter
#'
#' @description Simple progress counter for operations where full progress bar
#'     isn't needed but feedback is desired.
#'
#' @param current Integer current iteration
#' @param total Integer total iterations
#' @param operation Character operation name
#' @param interval Integer log every N iterations (default: 100)
#'
#' @return NULL (invisible)
#'
#' @keywords internal
log_progress <- function(
  current,
  total,
  operation = "Processing",
  interval = 100
) {
  if (current %% interval == 0 || current == total) {
    percent <- round(100 * current / total, 1)
    log_debug("%s: %d/%d (%.1f%%)", operation, current, total, percent)
  }
  invisible(NULL)
}
