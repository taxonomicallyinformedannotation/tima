start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

# step <- "process_spectra"
# paths <- parse_yaml_paths()
# params <- get_params(step = step)

log_debug(
  "This script",
  crayon::green("performs spectral similarity calculation. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

# process_spectra()
targets::tar_make(names = matches("annotations_spectral_merged"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
