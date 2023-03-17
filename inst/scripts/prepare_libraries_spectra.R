start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

# paths <- parse_yaml_paths()

log_debug(
  "This script",
  crayon::green("Prepares the internal library spectra \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

# prepare_libraries_spectra()
targets::tar_make(names = matches("library_spectra_internal_prepared"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
