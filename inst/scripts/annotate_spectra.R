start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

# step <- "annotate_spectra"
# paths <- parse_yaml_paths()
# params <- get_params(step = step)

log_debug(
  "This script",
  crayon::green(
    "performs spectral similarity calculation to annotate against a spectral library. \n"
  )
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

# annotate_spectra()
targets::tar_make(names = matches("annotations_spectral_is_l"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
