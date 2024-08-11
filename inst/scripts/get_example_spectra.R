start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("downloads an example of prepared MGF file. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

get_file(
  url = get_default_paths()$urls$examples$spectra,
  export = get_default_paths()$data$source$spectra
)

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
