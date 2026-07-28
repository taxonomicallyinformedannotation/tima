#' Safely Bind Rows of Data Frames
#'
#' @description Internal helper to bind multiple data frames together while
#'     avoiding warnings from tidytable when some inputs have row names.
#'     Removes row names before binding to ensure clean output.
#'
#' @details Accepts data frames either as separate arguments or as a single
#'     list of data frames. Each input is checked for row names and cleaned
#'     before binding to prevent tidytable binding warnings.
#'
#' @param ... Data frames to bind, or a single list of data frames.
#' @param .id Optional character string to create a new column with source IDs.
#'
#' @return A tidytable/data.frame with rows bound together, row names removed.
#'
#' @keywords internal
safe_bind_rows <- function(..., .id = NULL) {
  args <- list(...)
  # If a single list was passed (e.g., safe_bind_rows(list_of_dfs)) then unpack it
  if (length(args) == 1L && is.list(args[[1L]]) && !is.data.frame(args[[1L]])) {
    dfs <- args[[1L]]
  } else {
    dfs <- args
  }

  # Remove rownames from any data.frame-like inputs to avoid binding warnings
  dfs_clean <- lapply(dfs, function(x) {
    if (is.data.frame(x)) {
      # ensure plain data.frame or tibble without rownames
      rownames(x) <- NULL
      # preserve tbl_df classes if present
      x
    } else {
      x
    }
  })

  # Call tidytable::bind_rows robustly using do.call
  call_args <- dfs_clean
  if (!is.null(.id)) {
    call_args$.id <- .id
  }
  do.call(tidytable::bind_rows, call_args)
}
