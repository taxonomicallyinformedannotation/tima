start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("formats SIRIUS results")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

targets::tar_make(names = matches("^ann_sir_pre"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
