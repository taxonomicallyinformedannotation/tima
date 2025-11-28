start <- Sys.time()

library(tima)

log_trace(
  "This script prepares a library from prepared sub-libraries."
)
log_trace("Authors: AR")
log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("lib_sop_mer"))

end <- Sys.time()

log_success("Script finished in ", format(end - start))
