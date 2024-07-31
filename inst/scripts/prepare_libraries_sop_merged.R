start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("prepares a library from prepared sub-libraries. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

targets::tar_make(names = matches("lib_sop_mer"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
