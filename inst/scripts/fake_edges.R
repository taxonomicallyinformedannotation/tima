start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("fakes edges")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

step <- "fake_edges"
paths <- parse_yaml_paths()
params <- get_params(step = step)

fake_edges()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
