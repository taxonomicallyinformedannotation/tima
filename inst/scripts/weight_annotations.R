start <- Sys.time()

library(tima)

log_trace(
  "This script performs taxonomically informed scoring."
)
log_trace("Authors: AR, PMA")
log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("ann_pre$"))

end <- Sys.time()

log_success("Script finished in ", format(end - start))
