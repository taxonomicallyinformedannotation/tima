start <- Sys.time()

library(tima)

.log_trace <- getFromNamespace("log_trace", "tima")
.log_success <- getFromNamespace("log_success", "tima")

.log_trace(
  "This script prepares edges"
)
.log_trace("Authors: AR")
.log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("fea_edg_pre"))

end <- Sys.time()

.log_success("Script finished in %s", format(end - start))
