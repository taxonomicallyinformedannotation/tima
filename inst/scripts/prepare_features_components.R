start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

step <- "prepare_features_components"
paths <- parse_yaml_paths()
params <- get_params(step = step)

log_debug(
  "This script",
  crayon::green("prepares features metadata (mz, rt and component id)")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

prepare_features_components()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
