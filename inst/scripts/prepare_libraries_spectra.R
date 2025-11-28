start <- Sys.time()

library(tima)

log_trace(
  "This script prepares the internal library spectra."
)
log_trace("Authors: AR")
log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("lib_spe_exp_int_pre"))

end <- Sys.time()

log_success("Script finished in ", format(end - start))
