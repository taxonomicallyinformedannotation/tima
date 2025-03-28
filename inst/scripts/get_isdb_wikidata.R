start <- Sys.time()

library(tima)

logger::log_info(
  "This script downloads the In Silico DataBase (ISDB)"
)
logger::log_info("Authors: AR")
logger::log_info("Contributors: ...")

targets::tar_make(names = tidyselect::matches("lib_spe_is_wik"))

end <- Sys.time()

logger::log_info("Script finished in ", format(end - start))
