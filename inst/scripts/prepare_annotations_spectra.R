start <- Sys.time()

library(tima)

logger::log_trace(
  "This script formats spectral annotations results"
)
logger::log_trace("Authors: AR")
logger::log_trace("Contributors: ...")

## Not ann_spe because of `ann_spe_int` (see #69)
targets::tar_make(names = c(tidyselect::matches("^ann_spe_is")))

end <- Sys.time()

logger::log_success("Script finished in ", format(end - start))
