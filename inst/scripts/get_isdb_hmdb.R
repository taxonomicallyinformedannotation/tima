start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

paths <- parse_yaml_paths()

log_debug(
  "This script",
  crayon::green("downloads HMDB structures in silico fragmented. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

## TODO check md5 if possible (see https://twitter.com/Adafede/status/1592543895094788096)
log_debug("Downloading HMDB (might be long)")
download_file(
  url = paths$urls$hmdb$spectra$predicted,
  export = paths$data$source$libraries$spectra$hmdb_isdb
)

log_debug("Script finished in", crayon::green(format(end - start)))
