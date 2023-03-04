start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

# step <- "prepare_annotations_spectra"
# paths <- parse_yaml_paths()
# params <- get_params(step = step)

log_debug(
  "This script",
  crayon::green("formats ISDB results")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

# prepare_annotations_spectra()
targets::tar_make(names = matches("annotations_spectral_"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
