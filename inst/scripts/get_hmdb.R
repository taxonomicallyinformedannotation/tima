start <- Sys.time()

library(tima)

paths <- get_default_paths()

logger::log_trace(
  "This script downloads HMDB structures."
)
logger::log_trace("Authors: AR")
logger::log_trace("Contributors: ...")

## TODO check md5 if possible
## (see https://twitter.com/Adafede/status/1592543895094788096)
logger::log_trace("Downloading HMDB (might be long)")
get_file(
  url = paths$urls$hmdb$structures,
  export = paths$data$source$libraries$sop$hmdb
)

logger::log_success("Script finished in ", format(end - start))
