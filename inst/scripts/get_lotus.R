start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug("This script downloads LOTUS referenced structure-organism pairs. \n")
log_debug("Authors: AR")
log_debug("Contributors: ...")

paths <- parse_yaml_paths()

get_lotus()
