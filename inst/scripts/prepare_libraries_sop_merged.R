start <- Sys.time()

library(tima)

logger::log_trace(
  "This script prepares a library from prepared sub-libraries."
)
logger::log_trace("Authors: AR")
logger::log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("lib_sop_mer"))

end <- Sys.time()

logger::log_success("Script finished in ", format(end - start))
