start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("prepares a library from prepared sub-libraries. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

step <- "prepare_library"
paths <- parse_yaml_paths()
params <- get_params(step = step)

prepare_library()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
