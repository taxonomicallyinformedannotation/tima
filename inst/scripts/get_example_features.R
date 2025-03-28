start <- Sys.time()

library(tima)

logger::log_info(
  "This script downloads an example of minimal feature table"
)
logger::log_info("Authors: AR")
logger::log_info("Contributors: ...")

get_file(
  url = get_default_paths()$urls$examples$features,
  export = get_default_paths()$data$source$features
)

end <- Sys.time()

logger::log_info("Script finished in ", format(end - start))
