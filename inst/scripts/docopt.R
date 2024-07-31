start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)

step <- "params"
paths <- parse_yaml_paths()
params <- get_params(step = step)

log_debug(
  "This script",
  crayon::green("is a test script")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
