start <- Sys.time()

library(tima)

paths <- get_default_paths()

logger::log_trace(
  "This script downloads E. coli metabolome database structures."
)
logger::log_trace("Authors: AR")
logger::log_trace("Contributors: ...")

logger::log_trace("Downloading E. coli structures (should not take long)")
get_file(
  url = paths$urls$ecmdb$metabolites,
  export = paths$data$source$libraries$sop$ecmdb
)

logger::log_success("Script finished in ", format(end - start))
