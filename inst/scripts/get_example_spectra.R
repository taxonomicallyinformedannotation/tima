start <- Sys.time()

library(tima)

log_trace(
  "This script downloads an example of prepared MGF file."
)
log_trace("Authors: AR")
log_trace("Contributors: ...")

get_file(
  url = tima:::get_default_paths()$urls$examples$spectra,
  export = tima:::get_default_paths()$data$source$spectra
)

end <- Sys.time()

log_success("Script finished in ", format(end - start))
