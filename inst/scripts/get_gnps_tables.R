start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

# paths <- parse_yaml_paths()

log_debug(
  "This script",
  crayon::green("prepares adducts")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

# get_gnps_tables()
targets::tar_make(names = matches("gnps_tables"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
