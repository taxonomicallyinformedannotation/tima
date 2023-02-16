start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

step <- "prepare_config"
paths <- parse_yaml_paths()
params <- get_params(step = step)

log_debug(
  "This script",
  crayon::green("prepares parameters")
)
log_debug(
  "Authors: ",
  crayon::green("AR")
)
log_debug("Contributors: ...")

targets::tar_make(names = starts_with("config"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
