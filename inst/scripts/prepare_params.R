start <- Sys.time()

library(tima)

log_trace(
  "This script prepares parameters"
)
log_trace("Authors: AR")
log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("par_"))

end <- Sys.time()

log_success("Script finished in ", format(end - start))
