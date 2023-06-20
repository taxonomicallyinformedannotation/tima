start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

paths <- parse_yaml_paths()

log_debug(
  "This script",
  crayon::green("downloads E. coli metabolome database structures. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

log_debug("Downloading E. coli structures (should not take long)")
get_file(
  url = paths$urls$ecmdb$metabolites,
  export = paths$data$source$libraries$sop$ecmdb
)

log_debug("Script finished in", crayon::green(format(end - start)))
