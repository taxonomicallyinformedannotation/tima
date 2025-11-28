start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)

log_trace(
  "This script downloads HMDB structures in silico fragmented."
)
log_trace("Authors: AR")
log_trace("Contributors: ...")

## TODO check md5 if possible
## (see https://twitter.com/Adafede/status/1592543895094788096)
log_trace("Downloading HMDB (might be long)")
get_file(
  url = tima:::get_default_paths()$urls$hmdb$spectra$predicted,
  export = tima:::get_default_paths()$data$source$libraries$spectra$is$hmdb
)

log_success("Script finished in ", format(end - start))
