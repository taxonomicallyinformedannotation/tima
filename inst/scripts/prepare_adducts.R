start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

step <- "prepare_adducts"
paths <- parse_yaml_paths()
params <- get_params(step = step)

log_debug(
  "This script",
  crayon::green("prepares adducts")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

prepare_adducts()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
