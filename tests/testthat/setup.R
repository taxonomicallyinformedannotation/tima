library(package = "testthat", quietly = TRUE)
library(tima, quietly = TRUE) |> suppressMessages()

# Set logger threshold to reduce noise during tests
if (requireNamespace("logger", quietly = TRUE)) {
  tryCatch(
    expr = {
      logger::log_threshold(logger::WARN)
    },
    error = function(e) {
      # Ignore errors if logger setup fails
      invisible(NULL)
    }
  )
}
