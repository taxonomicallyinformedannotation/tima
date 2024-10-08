start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("downloads HMDB structures in silico fragmented. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

## TODO check md5 if possible
## (see https://twitter.com/Adafede/status/1592543895094788096)
log_debug("Downloading HMDB (might be long)")
get_file(
  url = get_default_paths()$urls$hmdb$spectra$predicted,
  export = get_default_paths()$data$source$libraries$spectra$is$hmdb
)

log_debug("Script finished in", crayon::green(format(end - start)))
