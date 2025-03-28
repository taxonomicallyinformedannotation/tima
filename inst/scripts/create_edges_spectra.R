start <- Sys.time()

library(tima)

logger::log_info(
  "This script performs spectral similarity calculation to create edges."
)
logger::log_info("Authors: AR")
logger::log_info("Contributors: CH, MS")

targets::tar_make(names = tidyselect::matches("fea_edg_spe"))

end <- Sys.time()

logger::log_info("Script finished in ", format(end - start))
