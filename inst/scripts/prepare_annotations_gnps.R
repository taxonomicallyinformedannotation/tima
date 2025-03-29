start <- Sys.time()

library(tima)

logger::log_trace(
  "This script formats GNPS results"
)
logger::log_trace("Authors: AR")
logger::log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("^ann_spe_exp_gnp_pre"))

end <- Sys.time()

logger::log_success("Script finished in ", format(end - start))
