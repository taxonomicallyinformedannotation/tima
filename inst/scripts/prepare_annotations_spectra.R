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
## Not ann_spe because of `ann_spe_int` (see #69)
targets::tar_make(names = c(matches("ann_spe_exp_g"), matches("ann_spe_is")))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
