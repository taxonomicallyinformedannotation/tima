start <- Sys.time()

library(tima)

log_trace(
  "This script prepares closed referenced structure-organism pairs."
)
log_trace("Authors: AR")
log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("lib_sop_clo_pre"))

end <- Sys.time()

log_success("Script finished in ", format(end - start))
