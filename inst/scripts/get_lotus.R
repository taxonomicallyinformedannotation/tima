start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

cat(
  "This script",
  crayon::green("downloads LOTUS referenced structure-organism pairs. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

paths <- parse_yaml_paths()

get_last_version_from_zenodo(
  doi = paths$url$lotus$doi,
  pattern = paths$urls$lotus$pattern,
  path = paths$data$source$libraries$lotus
)

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
