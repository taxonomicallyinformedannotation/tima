start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug(
  "This script prepares closed referenced structure-organism pairs \n",
  "for further processing. \n"
)
log_debug("Authors: AR")
log_debug("Contributors: ...")

step <- "prepare_closed"
paths <- parse_yaml_paths()
params <- get_params(step = step)

prepare_closed()

end <- Sys.time()

log_debug("Script finished in", format(end - start))
