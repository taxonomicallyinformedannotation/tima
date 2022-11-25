start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("downloads an example of prepared MGF file. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

get_example_mgf()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
