start <- Sys.time()

library(tima)

log_debug(
  "This script",
  crayon::green("informs taxonomically features")
)
log_debug(
  "Authors: ",
  crayon::green("AR"),
  ",",
  crayon::blue("PMA"),
  "\n"
)
log_debug("Contributors: ...")

targets::tar_make(names = tidyselect::matches("tax_pre"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
