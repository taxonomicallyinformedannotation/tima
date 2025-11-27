start <- Sys.time()

library(tima)

logger::log_trace(
  "This script downloads an example of minimal feature table"
)
logger::log_trace("Authors: AR")
logger::log_trace("Contributors: ...")

get_file(
  url = tima:::get_default_paths()$urls$examples$features,
  export = tima:::get_default_paths()$data$source$features
)

end <- Sys.time()

logger::log_success("Script finished in ", format(end - start))
