start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

cat(
  "This script",
  crayon::green("downloads the In Silico DataBase (ISDB) \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

paths <- parse_yaml_paths()

get_last_version_from_zenodo(
  doi = paths$url$isdb$doi,
  pattern = paths$urls$isdb$pattern$pos,
  path = paths$data$source$libraries$isdb$pos
)

get_last_version_from_zenodo(
  doi = paths$url$isdb$doi,
  pattern = paths$urls$isdb$pattern$neg,
  path = paths$data$source$libraries$isdb$neg
)

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
