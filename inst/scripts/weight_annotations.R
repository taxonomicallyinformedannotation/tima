start <- Sys.time()

library(tima)

logger::log_info(
  "This script performs taxonomically informed scoring."
)
logger::log_info("Authors: AR, PMA")
logger::log_info("Contributors: ...")

targets::tar_make(names = tidyselect::matches("ann_pre$"))

end <- Sys.time()

logger::log_info("Script finished in ", format(end - start))
