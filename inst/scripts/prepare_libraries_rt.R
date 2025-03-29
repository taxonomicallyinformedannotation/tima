start <- Sys.time()

library(tima)

logger::log_trace(
  "This script prepares a library of retention times from MGF or tabular data."
)
logger::log_trace("Authors: AR")
logger::log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("lib_rt"))

end <- Sys.time()

logger::log_success("Script finished in ", format(end - start))
