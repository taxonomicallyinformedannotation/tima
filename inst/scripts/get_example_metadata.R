start <- Sys.time()

library(tima)

log_debug(
  "This script",
  crayon::green("downloads an example of metadata table. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

get_file(
  url = get_default_paths()$urls$examples$metadata,
  export = get_default_paths()$data$source$metadata
)

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
