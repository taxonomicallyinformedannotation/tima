start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("Converts the predicted spectra from LOTUS \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

paths <- parse_yaml_paths()

prepare_isdb_lotus()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
