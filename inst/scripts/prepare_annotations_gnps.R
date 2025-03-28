start <- Sys.time()

library(tima)

logger::log_info(
  "This script formats GNPS results"
)
logger::log_info("Authors: AR")
logger::log_info("Contributors: ...")

targets::tar_make(names = tidyselect::matches("^ann_spe_exp_gnp_pre"))

end <- Sys.time()

logger::log_info("Script finished in ", format(end - start))
