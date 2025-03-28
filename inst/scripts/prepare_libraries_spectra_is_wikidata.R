start <- Sys.time()

library(tima)

logger::log_info(
  "This script prepares the predicted spectra from Wikidata. "
)
logger::log_info("Authors: AR")
logger::log_info("Contributors: ...")

targets::tar_make(names = tidyselect::matches("lib_spe_is_wik_pre"))

end <- Sys.time()

logger::log_info("Script finished in ", format(end - start))
