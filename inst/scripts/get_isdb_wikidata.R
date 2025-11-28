start <- Sys.time()

library(tima)

log_trace(
  "This script downloads the In Silico DataBase (ISDB)"
)
log_trace("Authors: AR")
log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("lib_spe_is_wik"))

end <- Sys.time()

log_success("Script finished in ", format(end - start))
