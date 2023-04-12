start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

# step <- "annotate_masses"
# paths <- parse_yaml_paths()
# params <- get_params(step = step)

log_debug(
  "This script",
  crayon::green("annotates based on exact mass (MS1)")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

# annotate_masses()
targets::tar_make(names = matches("ann_ms1_pre"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
