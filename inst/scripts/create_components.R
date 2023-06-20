start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

# step <- "create_components"
# paths <- parse_yaml_paths()
# params <- get_params(step = step)

log_debug(
  "This script",
  crayon::green("creates components from edges. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

# create_components()
targets::tar_make(names = matches("fea_com"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
