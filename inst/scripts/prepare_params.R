start <- Sys.time()

library(tima)

logger::log_trace(
  "This script prepares parameters"
)
logger::log_trace("Authors: AR")
logger::log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("par_"))

end <- Sys.time()

logger::log_success("Script finished in ", format(end - start))
