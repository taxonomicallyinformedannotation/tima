start <- Sys.time()

library(tima)

logger::log_info(
  "This script formats spectral annotations results"
)
logger::log_info("Authors: AR")
logger::log_info("Contributors: ...")

## Not ann_spe because of `ann_spe_int` (see #69)
targets::tar_make(names = c(tidyselect::matches("^ann_spe_is")))

end <- Sys.time()

logger::log_info("Script finished in ", format(end - start))
