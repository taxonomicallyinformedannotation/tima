start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

# step <- "process_annotations"
# paths <- parse_yaml_paths()
# params <- get_params(step = step)

log_debug(
  "This script performs",
  crayon::green("taxonomically informed scoring"),
  "and followed by",
  crayon::blue("chemical consistency informed scoring")
)
log_debug(
  "Authors: ",
  crayon::green("AR"),
  ",",
  crayon::blue("PMA"),
  "\n"
)
log_debug("Contributors: ...")

# process_annotations()
targets::tar_make(names = matches("annotations_processed"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
