start <- Sys.time()

library(tima)

.log_trace <- getFromNamespace("log_trace", "tima")
.log_success <- getFromNamespace("log_success", "tima")

.log_trace(
  "This script downloads HMDB structures in silico fragmented."
)
.log_trace("Authors: AR")
.log_trace("Contributors: ...")

## TODO check md5 if possible
## (see https://twitter.com/Adafede/status/1592543895094788096)
.log_trace("Downloading HMDB (might be long)")
get_file(
  url = getFromNamespace(
    "get_default_paths",
    "tima"
  )()$urls$hmdb$spectra$predicted,
  export = getFromNamespace(
    "get_default_paths",
    "tima"
  )()$data$source$libraries$spectra$is$hmdb
)

end <- Sys.time()

.log_success("Script finished in %s", format(end - start))
