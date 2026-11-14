start <- Sys.time()

library(tima)

.log_trace <- getFromNamespace("log_trace", "tima")
.log_success <- getFromNamespace("log_success", "tima")

paths <- getFromNamespace("get_default_paths", "tima")()

.log_trace(
  "This script downloads E. coli metabolome database structures."
)
.log_trace("Authors: AR")
.log_trace("Contributors: ...")

.log_trace("Downloading E. coli structures (should not take long)")
get_file(
  url = paths$urls$ecmdb$metabolites,
  export = paths$data$source$libraries$sop$ecmdb
)

end <- Sys.time()

.log_success("Script finished in %s", format(end - start))
