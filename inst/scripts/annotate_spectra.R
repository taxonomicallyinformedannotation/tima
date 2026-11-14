start <- Sys.time()

library(tima)

.log_trace <- getFromNamespace("log_trace", "tima")
.log_success <- getFromNamespace("log_success", "tima")

.log_trace(
  "This script performs spectral similarity calculation to annotate against a spectral library."
)
.log_trace("Authors: AR")
.log_trace("Contributors: ...")

targets::tar_make(names = tidyselect::matches("^ann_spe_(pos|neg)$"))

end <- Sys.time()

.log_success("Script finished in %s", format(end - start))
