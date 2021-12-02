start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug("This script creates adducts")
log_debug("Authors: AR")
log_debug("Contributors: ...")

step <- "prepare_adducts"
paths <- parse_yaml_paths()
params <- get_params(step = step)

prepare_adducts()

end <- Sys.time()

log_debug("Script finished in", format(end - start))
