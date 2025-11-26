suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(tima))

# Provide a reproducible temp root for this session's tests
.test_root <- file.path(tempdir(), sprintf("tima-tests-%s", Sys.getpid()))
dir.create(path = .test_root, recursive = TRUE, showWarnings = FALSE)

# Override default paths to use temp directories during tests
options(tima.test.interim_params_dir = file.path(.test_root, "params"))

# Reduce log noise in tests
logger::log_threshold(level = logger::ERROR)
