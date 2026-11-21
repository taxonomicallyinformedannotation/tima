start <- Sys.time()

library(tima)

.log_trace <- getFromNamespace("log_trace", "tima")
.log_success <- getFromNamespace("log_success", "tima")

.log_trace(
  "This script downloads an example of minimal feature table"
)
.log_trace("Authors: AR")
.log_trace("Contributors: ...")

paths <- getFromNamespace("get_default_paths", "tima")()
get_file(
  url = paths$urls$examples$features,
  export = paths$data$source$features
)

end <- Sys.time()

.log_success("Script finished in %s", format(end - start))
