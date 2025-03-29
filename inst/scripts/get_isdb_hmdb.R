start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)

logger::log_trace(
  "This script downloads HMDB structures in silico fragmented."
)
logger::log_trace("Authors: AR")
logger::log_trace("Contributors: ...")

## TODO check md5 if possible
## (see https://twitter.com/Adafede/status/1592543895094788096)
logger::log_trace("Downloading HMDB (might be long)")
get_file(
  url = get_default_paths()$urls$hmdb$spectra$predicted,
  export = get_default_paths()$data$source$libraries$spectra$is$hmdb
)

logger::log_success("Script finished in ", format(end - start))
