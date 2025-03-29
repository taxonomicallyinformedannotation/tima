start <- Sys.time()

library(tima)

logger::log_trace(
  "This script downloads an example of prepared MGF file."
)
logger::log_trace("Authors: AR")
logger::log_trace("Contributors: ...")

get_file(
  url = get_default_paths()$urls$examples$spectra,
  export = get_default_paths()$data$source$spectra
)

end <- Sys.time()

logger::log_success("Script finished in ", format(end - start))
