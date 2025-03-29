start <- Sys.time()

library(tima)

logger::log_trace(
  "This script downloads MassBank spectra"
)
logger::log_trace("Authors: AR")
logger::log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("lib_spe_exp_mb_raw"))

end <- Sys.time()

logger::log_success("Script finished in ", format(end - start))
