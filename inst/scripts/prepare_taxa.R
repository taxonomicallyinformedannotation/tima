start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("informs taxonomically features")
)
log_debug(
  "Authors: ",
  crayon::green("AR"),
  ",",
  crayon::blue("PMA"),
  "\n"
)
log_debug("Contributors: ...")

step <- "prepare_taxa"
paths <- parse_yaml_paths()
params <- get_params(step = step)

prepare_taxa()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
