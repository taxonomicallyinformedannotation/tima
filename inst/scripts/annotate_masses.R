start <- Sys.time()

library(tima)

log_trace(
  "This script annotates based on exact mass (MS1)"
)
log_trace("Authors: AR")
log_trace("Contributors: CH, MS")

targets::tar_make(names = tidyselect::matches("^ann_ms1_pre"))

end <- Sys.time()

log_success("Script finished in ", format(end - start))
