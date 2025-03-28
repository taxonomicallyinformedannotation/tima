start <- Sys.time()

library(tima)

logger::log_info(
  "This script downloads an example of metadata table."
)
logger::log_info("Authors: AR")
logger::log_info("Contributors: ...")

get_file(
  url = get_default_paths()$urls$examples$metadata,
  export = get_default_paths()$data$source$metadata
)

end <- Sys.time()

logger::log_info("Script finished in ", format(end - start))
