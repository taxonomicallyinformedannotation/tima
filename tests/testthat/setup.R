suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(tima))

# Detect if running under covr
is_covr <- ("covr" %in% loadedNamespaces()) ||
  identical(tolower(Sys.getenv("COVR")), "true")
options(tima.test.is_covr = is_covr)

# Provide a reproducible temp root for this session's tests
# CRITICAL: Always use tempdir(), never tests/testthat/data/
.test_root <- file.path(tempdir(), sprintf("tima-tests-%s", Sys.getpid()))
dir.create(.test_root, recursive = TRUE, showWarnings = FALSE)

# Helper to get fixture path (read-only access to fixtures)
fixture_path <- function(...) {
  # Get package root
  pkg_root <- tryCatch(
    rprojroot::find_package_root_file(),
    error = function(e) getwd()
  )
  file.path(pkg_root, "tests", "testthat", "fixtures", ...)
}

# Helper to copy fixture to temp location for modification
copy_fixture <- function(fixture_name, dest_name = NULL) {
  src <- fixture_path(fixture_name)
  if (!file.exists(src)) {
    stop("Fixture not found: ", fixture_name)
  }

  if (is.null(dest_name)) {
    dest_name <- basename(fixture_name)
  }

  dest <- file.path(tmp, dest_name)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  file.copy(src, dest, overwrite = TRUE)
  dest
}

# Optionally create a common tmp dir for convenience (legacy compatibility)
tmp <- tempdir()
tmp_dir <- tmp
temp_dir <- tmp

# Reduce log noise in tests
if (requireNamespace("logger", quietly = TRUE)) {
  try(logger::log_threshold(logger::WARN), silent = TRUE)
}

# Record package root for helpers that need it
options(tima.test.pkg_root = normalizePath(getwd()))
options(tima.test.temp_dir = tmp)

# Ensure test writes NEVER go to tests/testthat/data/
# Override any potential writes by ensuring paths are redirected
.prevent_testthat_data_writes <- function() {
  testthat_data <- file.path(getwd(), "tests", "testthat", "data")
  if (dir.exists(testthat_data)) {
    warning(
      "tests/testthat/data/ exists but should not be used for test outputs. ",
      "All test data should go to tempdir().",
      call. = FALSE
    )
  }
}
.prevent_testthat_data_writes()
