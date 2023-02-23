start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

paths <- parse_yaml_paths()

log_debug(
  "This script",
  crayon::green("downloads an example of SIRIUS output. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

download_file(
  url = paths$urls$examples$sirius,
  export = paths$data$interim$annotations$example_sirius
)
message("Unzipping")
utils::unzip(
  zipfile = paths$data$interim$annotations$example_sirius,
  exdir = dirname(paths$data$interim$annotations$example_sirius)
)

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
