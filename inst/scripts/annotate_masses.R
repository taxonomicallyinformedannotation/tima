start <- Sys.time()

library(tima)

logger::log_trace(
  "This script annotates based on exact mass (MS1)"
)
logger::log_trace("Authors: AR")
logger::log_trace("Contributors: CH, MS")

targets::tar_make(names = tidyselect::matches("^ann_ms1_pre"))

end <- Sys.time()

logger::log_success("Script finished in ", format(end - start))
