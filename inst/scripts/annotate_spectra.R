start <- Sys.time()

library(tima)

logger::log_info(
  "This script performs spectral similarity calculation to annotate against a spectral library."
)
logger::log_info("Authors: AR")
logger::log_info("Contributors: ...")

targets::tar_make(names = tidyselect::matches("^ann_spe_pre"))

end <- Sys.time()

logger::log_info("Script finished in ", format(end - start))
