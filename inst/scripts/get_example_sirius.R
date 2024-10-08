start <- Sys.time()

library(tima)

log_debug(
  "This script",
  crayon::green("downloads an example of SIRIUS output. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

get_example_sirius()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
