start <- Sys.time()

library(tima)

logger::log_trace(
  "This script performs spectral similarity calculation to create edges."
)
logger::log_trace("Authors: AR")
logger::log_trace("Contributors: CH, MS")

targets::tar_make(names = tidyselect::matches("fea_edg_spe"))

end <- Sys.time()

logger::log_success("Script finished in ", format(end - start))
