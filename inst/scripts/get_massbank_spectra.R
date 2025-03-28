start <- Sys.time()

library(tima)

logger::log_info(
  "This script downloads MassBank spectra"
)
logger::log_info("Authors: AR")
logger::log_info("Contributors: ...")

targets::tar_make(names = tidyselect::matches("lib_spe_exp_mb_raw"))

end <- Sys.time()

logger::log_info("Script finished in ", format(end - start))
