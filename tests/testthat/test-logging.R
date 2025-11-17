# ==============================================================================
# Test Suite: Logging configuration
# ==============================================================================

with_temp_env <- function(vars, code) {
  old <- lapply(names(vars), Sys.getenv)
  on.exit(
    {
      for (i in seq_along(vars)) {
        Sys.setenv(structure(old[[i]], names = names(vars)[i]))
      }
    },
    add = TRUE
  )
  do.call(Sys.setenv, as.list(vars))
  force(code)
}

# test_that("init_logging respects TIMA_LOG_FILE env var", {
#   tmp <- tempfile("tima-log-", fileext = ".log")
#   unlink(tmp)
#
#   with_temp_env(c(TIMA_LOG_FILE = tmp, TIMA_LOG_LEVEL = "INFO"), {
#     tima:::init_logging()
#     logger::log_info("test message")
#   })
#
#   expect_true(file.exists(tmp))
#   content <- readLines(tmp, warn = FALSE)
#   expect_true(any(grepl("test message", content)))
#   unlink(tmp)
# })

# test_that("init_logging falls back on invalid level", {
#   tmp <- tempfile("tima-log-", fileext = ".log")
#   unlink(tmp)
#
#   expect_warning(
#     with_temp_env(c(TIMA_LOG_FILE = tmp, TIMA_LOG_LEVEL = "NOPE"), {
#       tima:::init_logging()
#       logger::log_info("hello")
#     }),
#     "Unknown TIMA_LOG_LEVEL"
#   )
#
#   expect_true(file.exists(tmp))
#   unlink(tmp)
# })
