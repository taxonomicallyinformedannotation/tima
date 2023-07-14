start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("downloads the In Silico DataBase (ISDB) \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

targets::tar_make(names = matches("lib_spe_is_lot"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
