start <- Sys.time()

library(tima)

logger::log_info(
  "This script formats SIRIUS results"
)
logger::log_info("Authors: AR")
logger::log_info("Contributors: ...")

targets::tar_make(names = tidyselect::matches("^ann_sir_pre"))

end <- Sys.time()

logger::log_info("Script finished in ", format(end - start))
