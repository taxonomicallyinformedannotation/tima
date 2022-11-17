start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("is made for testing adducts")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

step <- "parse_cli_params"
paths <- parse_yaml_paths()
params <- get_params(step = step)

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
