start <- Sys.time()

library(tima)

log_trace(
  "This script downloads an example of minimal feature table"
)
log_trace("Authors: AR")
log_trace("Contributors: ...")

get_file(
  url = getFromNamespace("get_default_paths", "tima")()$urls$examples$features,
  export = getFromNamespace("get_default_paths", "tima")()$data$source$features
)

end <- Sys.time()

log_success("Script finished in ", format(end - start))
