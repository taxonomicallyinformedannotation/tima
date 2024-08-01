start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("creates components from edges. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

targets::tar_make(names = matches("fea_com"), script = system.file("pipelines/_targets.R", package = "tima"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
