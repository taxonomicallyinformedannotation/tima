start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("prepares features metadata (mz, rt) and fakes component id)")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

step <- "fake_features_components"
paths <- parse_yaml_paths()
params <- get_params(step = step)

fake_features_components()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
