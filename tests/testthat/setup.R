suppressPackageStartupMessages(library(testthat))
# Load package code once for all tests; internal helpers become directly callable.
suppressPackageStartupMessages(devtools::load_all(
  path = ".",
  export_all = TRUE,
  quiet = TRUE
))

# Provide a reproducible temp root for this session's tests
.test_root <- file.path(tempdir(), sprintf("tima-tests-%s", Sys.getpid()))
dir.create(path = .test_root, recursive = TRUE, showWarnings = FALSE)
options(tima.test_root = .test_root)
options(tima.test.interim_params_dir = file.path(.test_root, "params"))

# Reduce log noise in tests
# Set threshold to ERROR level (200) to minimize test output
lgr::lgr$set_threshold(200)
