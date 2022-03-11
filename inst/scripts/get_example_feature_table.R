start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("downloads an example of minimal feature table \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

#' TODO CLI DOCOPT

get_example_feature_table()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
