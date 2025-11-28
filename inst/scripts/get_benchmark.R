start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)

paths <- tima:::get_default_paths()

log_trace(
  "This script downloads benchmarking set from matchms work on GNPS dataset."
)
log_trace("Authors: AR")
log_trace("Contributors: ...")

get_file(
  url = paths$urls$benchmarking_set,
  export = paths$data$source$benchmark$set
)

end <- Sys.time()

log_success("Script finished in ", format(end - start))
