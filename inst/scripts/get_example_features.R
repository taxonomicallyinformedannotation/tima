start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("downloads an example of minimal feature table \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

get_file(
  url = parse_yaml_paths()$urls$examples$features,
  export = parse_yaml_paths()$data$source$features
)

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
