start <- Sys.time()

library(tima)

paths <- get_default_paths()

logger::log_info(
  "This script downloads E. coli metabolome database structures."
)
logger::log_info("Authors: AR")
logger::log_info("Contributors: ...")

logger::log_info("Downloading E. coli structures (should not take long)")
get_file(
  url = paths$urls$ecmdb$metabolites,
  export = paths$data$source$libraries$sop$ecmdb
)

logger::log_info("Script finished in ", format(end - start))
