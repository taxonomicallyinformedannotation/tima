start <- Sys.time()

library(tima)

log_debug(
  "This script",
  crayon::green("gets gnps tables")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

targets::tar_make(names = tidyselect::matches("gnps_tables"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
