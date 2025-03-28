start <- Sys.time()

library(tima)

logger::log_info(
  "This script downloads an example of prepared MGF file."
)
logger::log_info("Authors: AR")
logger::log_info("Contributors: ...")

get_file(
  url = get_default_paths()$urls$examples$spectra,
  export = get_default_paths()$data$source$spectra
)

end <- Sys.time()

logger::log_info("Script finished in ", format(end - start))
