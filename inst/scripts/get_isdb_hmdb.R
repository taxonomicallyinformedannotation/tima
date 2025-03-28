start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)

logger::log_info(
  "This script downloads HMDB structures in silico fragmented."
)
logger::log_info("Authors: AR")
logger::log_info("Contributors: ...")

## TODO check md5 if possible
## (see https://twitter.com/Adafede/status/1592543895094788096)
logger::log_info("Downloading HMDB (might be long)")
get_file(
  url = get_default_paths()$urls$hmdb$spectra$predicted,
  export = get_default_paths()$data$source$libraries$spectra$is$hmdb
)

logger::log_info("Script finished in ", format(end - start))
