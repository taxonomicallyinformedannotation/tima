start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)

step <- "params"
paths <- tima:::get_default_paths()
params <- tima:::get_params(step = step)

log_trace(
  "This script is a test script"
)
log_trace("Authors: AR")
log_trace("Contributors: ...")

end <- Sys.time()

log_success("Script finished in ", format(end - start))
