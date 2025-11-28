start <- Sys.time()

library(tima)

log_trace(
  "This script performs annotation filtering based on retention time matching."
)
log_trace("Authors: AR")
log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("^ann_fil"))

end <- Sys.time()

log_success("Script finished in ", format(end - start))
