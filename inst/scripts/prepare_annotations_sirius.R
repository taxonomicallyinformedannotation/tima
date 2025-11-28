start <- Sys.time()

library(tima)

log_trace(
  "This script formats SIRIUS results"
)
log_trace("Authors: AR")
log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("^ann_sir_pre"))

end <- Sys.time()

log_success("Script finished in ", format(end - start))
