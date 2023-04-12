start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

# step <- "prepare_libraries_sop_merged"
# paths <- parse_yaml_paths()
# params <- get_params(step = step)

log_debug(
  "This script",
  crayon::green("prepares a library from prepared sub-libraries. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

# prepare_libraries_sop_merged()
targets::tar_make(names = matches("lib_sop_mer"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
