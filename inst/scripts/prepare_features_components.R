start <- Sys.time()

library(tima)

log_debug(
  "This script",
  crayon::green("prepares features metadata (mz, rt and component id)")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

targets::tar_make(names = matches("fea_com_pre"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
