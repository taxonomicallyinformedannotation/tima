start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

# paths <- parse_yaml_paths()

log_debug(
  "This script",
  crayon::green("prepares LOTUS referenced structure-organism pairs \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

# prepare_lotus()
targets::tar_make(names = matches("library_sop_lotus_prepared"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
