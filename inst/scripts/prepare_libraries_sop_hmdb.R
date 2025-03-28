start <- Sys.time()

library(tima)

logger::log_info(
  "This script prepares hmdb structure-organism pairs."
)
logger::log_info("Authors: AR")
logger::log_info("Contributors: ...")

targets::tar_make(names = tidyselect::matches("lib_sop_hmd_pre"))

end <- Sys.time()

logger::log_info("Script finished in ", format(end - start))
