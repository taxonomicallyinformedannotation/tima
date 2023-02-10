start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

paths <- parse_yaml_paths()

log_debug(
  "This script",
  crayon::green("downloads benchmarking set from matchms work on GNPS dataset. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

get_benchmark()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
