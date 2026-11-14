start <- Sys.time()

library(tima)

.log_trace <- getFromNamespace("log_trace", "tima")
.log_success <- getFromNamespace("log_success", "tima")

.log_trace(
  "This script downloads an example of SIRIUS output."
)
.log_trace("Authors: AR")
.log_trace("Contributors: ...")

getFromNamespace("get_example_sirius", "tima")()

end <- Sys.time()

.log_success("Script finished in %s", format(end - start))
