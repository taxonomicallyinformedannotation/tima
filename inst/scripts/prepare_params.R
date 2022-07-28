start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("prepares parameters")
)
log_debug(
  "Authors: ",
  crayon::green("AR")
)
log_debug("Contributors: ...")

step <- "prepare_params"
paths <- parse_yaml_paths()
params <- get_params(step = step)

prepare_params()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
