start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)

step <- "params"
paths <- getFromNamespace("get_default_paths", "tima")()
params <- getFromNamespace("get_params", "tima")(step = step)

log_trace(
  "This script is a test script"
)
log_trace("Authors: AR")
log_trace("Contributors: ...")

end <- Sys.time()

log_success("Script finished in ", format(end - start))
