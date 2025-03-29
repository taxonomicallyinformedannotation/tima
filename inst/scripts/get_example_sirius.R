start <- Sys.time()

library(tima)

logger::log_trace(
  "This script downloads an example of SIRIUS output."
)
logger::log_trace("Authors: AR")
logger::log_trace("Contributors: ...")

get_example_sirius()

end <- Sys.time()

logger::log_success("Script finished in ", format(end - start))
