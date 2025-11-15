#' Package startup
#'
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Initialize logging according to environment variables.
  # Safe if reticulate/logger not yet configured; the function sets layout/appender.
  # Users can override later in their scripts.
  try(
    {
      init_logging()
    },
    silent = TRUE
  )
}
