start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("formats ISDB results")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

step <- "prepare_isdb"
paths <- parse_yaml_paths()
params <- get_params(step = step)

prepare_isdb()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
