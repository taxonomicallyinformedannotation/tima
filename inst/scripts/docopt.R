start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)

step <- "params"
paths <- tima:::get_default_paths()
params <- tima:::get_params(step = step)

logger::log_trace(
  "This script is a test script"
)
logger::log_trace("Authors: AR")
logger::log_trace("Contributors: ...")

end <- Sys.time()

logger::log_success("Script finished in ", format(end - start))
