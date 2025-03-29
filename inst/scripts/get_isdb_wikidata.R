start <- Sys.time()

library(tima)

logger::log_trace(
  "This script downloads the In Silico DataBase (ISDB)"
)
logger::log_trace("Authors: AR")
logger::log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("lib_spe_is_wik"))

end <- Sys.time()

logger::log_success("Script finished in ", format(end - start))
