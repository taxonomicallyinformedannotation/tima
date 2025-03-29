start <- Sys.time()

library(tima)

logger::log_trace(
  "This script informs taxonomically features"
)
logger::log_trace("Authors: AR, PMA")
logger::log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("tax_pre"))

end <- Sys.time()

logger::log_success("Script finished in ", format(end - start))
