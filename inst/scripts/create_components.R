start <- Sys.time()

library(tima)

logger::log_trace(
  "This script creates components from edges."
)
logger::log_trace("Authors: AR")
logger::log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("fea_com"))

end <- Sys.time()

logger::log_success("Script finished in ", format(end - start))
