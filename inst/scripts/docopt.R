start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)

step <- "params"
paths <- get_default_paths()
params <- get_params(step = step)

logger::log_info(
  "This script is a test script"
)
logger::log_info("Authors: AR")
logger::log_info("Contributors: ...")

end <- Sys.time()

logger::log_info("Script finished in ", format(end - start))
