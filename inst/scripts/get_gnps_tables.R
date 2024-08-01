start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("gets gnps tables")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

targets::tar_make(names = matches("gnps_tables"), script = system.file("/pipelines/_targets.R", "tima"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
