start <- Sys.time()

library(tima)

logger::log_trace(
  "This script gets gnps tables"
)
logger::log_trace("Authors: AR")
logger::log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("gnps_tables"))

end <- Sys.time()

logger::log_success("Script finished in ", format(end - start))
