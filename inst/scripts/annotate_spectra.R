start <- Sys.time()

library(tima)

logger::log_trace(
  "This script performs spectral similarity calculation to annotate against a spectral library."
)
logger::log_trace("Authors: AR")
logger::log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("^ann_spe_pre"))

end <- Sys.time()

logger::log_success("Script finished in ", format(end - start))
