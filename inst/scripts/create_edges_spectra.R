start <- Sys.time()

library(tima)

log_trace(
  "This script performs spectral similarity calculation to create edges."
)
log_trace("Authors: AR")
log_trace("Contributors: CH, MS")

targets::tar_make(names = tidyselect::matches("fea_edg_spe"))

end <- Sys.time()

log_success("Script finished in ", format(end - start))
