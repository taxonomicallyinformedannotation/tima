start <- Sys.time()

library(tima)

log_debug(
  "This script",
  crayon::green("prepares LOTUS referenced structure-organism pairs \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

targets::tar_make(names = tidyselect::matches("lib_sop_lot_pre"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
