start <- Sys.time()

library(tima)

paths <- tima:::get_default_paths()

log_trace(
  "This script downloads E. coli metabolome database structures."
)
log_trace("Authors: AR")
log_trace("Contributors: ...")

log_trace("Downloading E. coli structures (should not take long)")
get_file(
  url = paths$urls$ecmdb$metabolites,
  export = paths$data$source$libraries$sop$ecmdb
)

log_success("Script finished in ", format(end - start))
