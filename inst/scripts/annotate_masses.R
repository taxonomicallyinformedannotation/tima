start <- Sys.time()

library(tima)

logger::log_info(
  "This script annotates based on exact mass (MS1)"
)
logger::log_info("Authors: AR")
logger::log_info("Contributors: CH, MS")

targets::tar_make(names = tidyselect::matches("^ann_ms1_pre"))

end <- Sys.time()

logger::log_info("Script finished in ", format(end - start))
