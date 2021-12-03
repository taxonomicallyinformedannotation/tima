start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("prepares closed referenced structure-organism pairs \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

step <- "prepare_closed"
paths <- parse_yaml_paths()
params <- get_params(step = step)

prepare_closed()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
