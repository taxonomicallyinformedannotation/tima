start <- Sys.time()

library(tima)

logger::log_info(
  "This script prepares a library of retention times from MGF or tabular data."
)
logger::log_info("Authors: AR")
logger::log_info("Contributors: ...")

targets::tar_make(names = tidyselect::matches("lib_rt"))

end <- Sys.time()

logger::log_info("Script finished in ", format(end - start))
