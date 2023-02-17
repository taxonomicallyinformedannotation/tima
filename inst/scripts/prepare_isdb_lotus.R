start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

# paths <- parse_yaml_paths()

log_debug(
  "This script",
  crayon::green("Converts the predicted spectra from LOTUS \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

# prepare_isdb_lotus()
targets::tar_make(names = starts_with("library_spectra_is_lotus_prepared"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
