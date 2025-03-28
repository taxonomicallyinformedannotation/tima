start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)

paths <- get_default_paths()

logger::log_info(
  "This script downloads benchmarking set from matchms work on GNPS dataset."
)
logger::log_info("Authors: AR")
logger::log_info("Contributors: ...")

get_file(
  url = paths$urls$benchmarking_set,
  export = paths$data$source$benchmark$set
)

end <- Sys.time()

logger::log_info("Script finished in ", format(end - start))
