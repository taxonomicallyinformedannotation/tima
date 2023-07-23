start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("formats spectral annotations results")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

## Not ann_spe because of `ann_spe_int` (see #69)
targets::tar_make(names = c(matches("^ann_spe_is")))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
