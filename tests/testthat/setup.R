suppressPackageStartupMessages(library(testthat))

# Minimize logging noise in tests (including lazy logger initialization)
Sys.setenv(
  TIMA_LOG_LEVEL = "ERROR",
  TIMA_LOG_FILE = file.path(
    tempdir(),
    sprintf("tima-test-%s.log", Sys.getpid())
  ),
  TIMA_LOG_CONSOLE = "FALSE"
)

# Keep setup compatible with both load-all style and testthat::test_package().
# test_package() runs against an installed namespace.
if (!isTRUE(requireNamespace("tima", quietly = TRUE))) {
  if (isTRUE(requireNamespace("pkgload", quietly = TRUE))) {
    suppressPackageStartupMessages(pkgload::load_all(
      path = ".",
      export_all = TRUE,
      quiet = TRUE
    ))
  } else {
    stop(
      "Neither installed package 'tima' nor 'pkgload' is available for test setup."
    )
  }
}

# Provide a reproducible temp root for this session's tests
.test_root <- file.path(tempdir(), sprintf("tima-tests-%s", Sys.getpid()))
dir.create(path = .test_root, recursive = TRUE, showWarnings = FALSE)
options(tima.test_root = .test_root)
options(tima.test.interim_params_dir = file.path(.test_root, "params"))

# Reduce log noise in tests
# Set threshold to ERROR level (200) to minimize test output
lgr::lgr$set_threshold(200)

# Ensure lazy-init logger stays quiet and does not emit to console.
if (isTRUE(requireNamespace("tima", quietly = TRUE))) {
  getFromNamespace("init_logging", "tima")()
  appenders <- lgr::lgr$appenders
  is_file <- vapply(appenders, inherits, logical(1), what = "AppenderFile")
  lgr::lgr$set_appenders(appenders[is_file])
  lgr::lgr$set_threshold(200)
}
