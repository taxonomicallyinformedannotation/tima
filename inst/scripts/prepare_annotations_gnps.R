start <- Sys.time()

library(tima)

log_trace(
  "This script formats GNPS results"
)
log_trace("Authors: AR")
log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("^ann_spe_exp_gnp_pre"))

end <- Sys.time()

log_success("Script finished in ", format(end - start))
