start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

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

targets::tar_make(names = matches("tax_pre"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
