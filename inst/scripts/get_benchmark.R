start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)

paths <- get_default_paths()

log_debug(
  "This script",
  crayon::green(
    "downloads benchmarking set",
    "from matchms work on GNPS dataset. \n"
  )
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

get_file(
  url = paths$urls$benchmarking_set,
  export = paths$data$source$benchmark$set
)

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
