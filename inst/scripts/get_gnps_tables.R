start <- Sys.time()

library(tima)

log_trace(
  "This script gets gnps tables"
)
log_trace("Authors: AR")
log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("gnps_tables"))

end <- Sys.time()

log_success("Script finished in ", format(end - start))
