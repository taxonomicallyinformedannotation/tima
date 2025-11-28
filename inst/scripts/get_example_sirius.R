start <- Sys.time()

library(tima)

log_trace(
  "This script downloads an example of SIRIUS output."
)
log_trace("Authors: AR")
log_trace("Contributors: ...")

get_example_sirius()

end <- Sys.time()

log_success("Script finished in ", format(end - start))
