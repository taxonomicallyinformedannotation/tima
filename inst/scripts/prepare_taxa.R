start <- Sys.time()

library(tima)

log_trace(
  "This script informs taxonomically features"
)
log_trace("Authors: AR, PMA")
log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("tax_pre"))

end <- Sys.time()

log_success("Script finished in ", format(end - start))
