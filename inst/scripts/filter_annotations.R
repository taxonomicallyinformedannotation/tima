start <- Sys.time()

library(tima)

logger::log_trace(
  "This script performs annotation filtering based on retention time matching."
)
logger::log_trace("Authors: AR")
logger::log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("^ann_fil"))

end <- Sys.time()

logger::log_success("Script finished in ", format(end - start))
