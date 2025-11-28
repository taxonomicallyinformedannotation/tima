start <- Sys.time()

library(tima)

log_trace(
  "This script creates components from edges."
)
log_trace("Authors: AR")
log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("fea_com"))

end <- Sys.time()

log_success("Script finished in ", format(end - start))
