start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("performs spectral similarity calculation to create edges. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ", crayon::blue("CH"), crayon::cyan("MS"), "\n")

targets::tar_make(names = matches("fea_edg_spe"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
