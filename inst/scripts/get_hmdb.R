start <- Sys.time()

library(tima)

paths <- get_default_paths()

logger::log_info(
  "This script downloads HMDB structures."
)
logger::log_info("Authors: AR")
logger::log_info("Contributors: ...")

## TODO check md5 if possible
## (see https://twitter.com/Adafede/status/1592543895094788096)
logger::log_info("Downloading HMDB (might be long)")
get_file(
  url = paths$urls$hmdb$structures,
  export = paths$data$source$libraries$sop$hmdb
)

logger::log_info("Script finished in ", format(end - start))
