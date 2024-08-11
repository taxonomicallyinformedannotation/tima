start <- Sys.time()

library(tima)

log_debug(
  "This script",
  crayon::green("prepares parameters")
)
log_debug("Authors: ", crayon::green("AR"))
log_debug("Contributors: ...")

targets::tar_make(names = matches("par_"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
