# Helper: bind rows safely by removing row names from inputs
# This avoids warnings from dplyr/tidytable when some data.frames have rownames
# Usage: safe_bind_rows(df_list) or safe_bind_rows(df1, df2, ...)
# Internal helper, not exported
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
  if (!is.null(.id)) call_args$.id <- .id
  do.call(tidytable::bind_rows, call_args)
}
