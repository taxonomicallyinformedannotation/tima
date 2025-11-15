# Test fixtures and helpers

#' Setup an isolated temporary project directory for filesystem tests
#'
#' - Creates a temp directory and switches the working directory to it
#' - Copies the package backbone into the temp directory (data structure)
#' - Returns normalized default paths for convenience
#'
#' Use inside tests: paths <- local_test_project()
local_test_project <- function(copy = TRUE) {
  withr::local_tempdir() -> tmp
  withr::local_dir(tmp)

  # Quiet logs during tests
  if (requireNamespace("logger", quietly = TRUE)) {
    logger::log_threshold(logger::WARN)
  }

  if (isTRUE(copy)) {
    copy_backbone(cache_dir = ".")
  }

  get_default_paths()
}

#' Quiet logging within a test scope
local_quiet_logging <- function(threshold = "WARN") {
  if (requireNamespace("logger", quietly = TRUE)) {
    # Convert character to logger level if needed
    lvl <- tryCatch(get(threshold, envir = asNamespace("logger")), error = function(...) threshold)
    logger::log_threshold(lvl)
  }
  invisible(NULL)
}

