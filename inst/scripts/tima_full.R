start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("does everything you ever dreamt of. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ", crayon::blue("PMA"), "\n")

targets::tar_make(names = matches("annotations_prepared_crazy"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
