start <- Sys.time()

library(tima)

log_debug(
  "This script",
  crayon::green("formats GNPS results")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

targets::tar_make(names = tidyselect::matches("^ann_spe_exp_gnp_pre"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
