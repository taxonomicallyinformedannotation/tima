start <- Sys.time()

library(tima)

.log_trace <- getFromNamespace("log_trace", "tima")
.log_success <- getFromNamespace("log_success", "tima")

paths <- getFromNamespace("get_default_paths", "tima")()

.log_trace(
  "This script downloads benchmarking set from matchms work on GNPS dataset."
)
.log_trace("Authors: AR")
.log_trace("Contributors: ...")

get_file(
  url = paths$urls$benchmarking_set,
  export = paths$data$source$benchmark$set
)

end <- Sys.time()

.log_success("Script finished in %s", format(end - start))
