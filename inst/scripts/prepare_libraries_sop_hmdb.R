start <- Sys.time()

library(tima)

log_trace(
  "This script prepares hmdb structure-organism pairs."
)
log_trace("Authors: AR")
log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("lib_sop_hmd_pre"))

end <- Sys.time()

log_success("Script finished in ", format(end - start))
