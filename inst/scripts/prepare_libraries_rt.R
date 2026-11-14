start <- Sys.time()

library(tima)

.log_trace <- getFromNamespace("log_trace", "tima")
.log_success <- getFromNamespace("log_success", "tima")

.log_trace(
  "This script prepares a library of retention times from MGF or tabular data."
)
.log_trace("Authors: AR")
.log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("lib_rt"))

end <- Sys.time()

.log_success("Script finished in %s", format(end - start))
