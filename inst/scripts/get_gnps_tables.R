start <- Sys.time()

library(tima)

logger::log_info(
  "This script gets gnps tables"
)
logger::log_info("Authors: AR")
logger::log_info("Contributors: ...")

targets::tar_make(names = tidyselect::matches("gnps_tables"))

end <- Sys.time()

logger::log_info("Script finished in ", format(end - start))
