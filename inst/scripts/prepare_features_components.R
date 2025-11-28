start <- Sys.time()

library(tima)

log_trace(
  "This script prepares features metadata (mz, rt and component id)"
)
log_trace("Authors: AR")
log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("fea_com_pre"))

end <- Sys.time()

log_success("Script finished in ", format(end - start))
