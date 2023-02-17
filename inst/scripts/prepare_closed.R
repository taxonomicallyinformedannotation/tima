start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

# step <- "prepare_closed"
# paths <- parse_yaml_paths()
# params <- get_params(step = step)

log_debug(
  "This script",
  crayon::green("prepares closed referenced structure-organism pairs \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

# prepare_closed()
targets::tar_make(names = matches("library_sop_closed_prepared"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
