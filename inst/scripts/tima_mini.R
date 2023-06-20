start <- Sys.time()

library("timaR")

log_debug(
  "This script",
  crayon::green("does everything you ever dreamt of. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ", crayon::blue("PMA"), "\n")

targets::tar_make(names = matches("ann_pre$"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
