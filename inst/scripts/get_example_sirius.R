start <- Sys.time()

library(tima)

logger::log_info(
  "This script downloads an example of SIRIUS output."
)
logger::log_info("Authors: AR")
logger::log_info("Contributors: ...")

get_example_sirius()

end <- Sys.time()

logger::log_info("Script finished in ", format(end - start))
