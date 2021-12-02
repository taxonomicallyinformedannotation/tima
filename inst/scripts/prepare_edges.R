start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug("This script takes a list of edges and formats it for later on")
log_debug("Authors: AR")
log_debug("Contributors: ...")

step <- "prepare_edges"
paths <- parse_yaml_paths()
params <- get_params(step = step)

prepare_edges()

end <- Sys.time()

log_debug("Script finished in", format(end - start))
