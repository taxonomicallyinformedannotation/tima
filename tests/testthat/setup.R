suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(tima))

# Detect if running under covr
is_covr <- ("covr" %in% loadedNamespaces()) ||
  identical(tolower(Sys.getenv("COVR")), "true")
options(tima.test.is_covr = is_covr)

# Provide a reproducible temp root for this session's tests
.test_root <- file.path(tempdir(), sprintf("tima-tests-%s", Sys.getpid()))
dir.create(.test_root, recursive = TRUE, showWarnings = FALSE)

# Helper to create isolated temp directories per test
make_tmp_dir <- function(subdir = NULL) {
  dir_name <- if (is.null(subdir)) {
    tempfile("test-", tmpdir = .test_root)
  } else {
    file.path(.test_root, subdir)
  }
  dir.create(dir_name, recursive = TRUE, showWarnings = FALSE)
  dir_name
}

# Helper to create isolated temp files in the temp dirs
make_tmp_file <- function(name = NULL, fileext = "", subdir = NULL) {
  dir_name <- make_tmp_dir(subdir)
  if (is.null(name)) {
    file.path(dir_name, paste0(tempfile("", tmpdir = NULL), fileext))
  } else {
    file.path(dir_name, paste0(name, fileext))
  }
}

# Optionally create a common tmp dir for convenience (legacy compatibility)
tmp <- make_tmp_dir("tmp")
tmp_dir <- tmp
temp_dir <- tmp

# Reduce log noise in tests
if (requireNamespace("logger", quietly = TRUE)) {
  try(logger::log_threshold(logger::WARN), silent = TRUE)
}

# Record package root for helpers that need it
options(tima.test.pkg_root = normalizePath(getwd()))
options(tima.test.temp_dir = tmp)

# Example usage inside a test
# minimal <- tidytable::tidytable(
#   structure_inch_
