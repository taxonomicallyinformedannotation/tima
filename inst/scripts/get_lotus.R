start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)


log_debug(
  "This script",
  crayon::green("downloads LOTUS referenced structure-organism pairs. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

targets::tar_make(names = matches("lib_sop_lot$"), script = system.file("pipelines/_targets.R", package = "tima"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
