start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

# step <- "prepare_taxa"
# paths <- parse_yaml_paths()
# params <- get_params(step = step)

log_debug(
  "This script",
  crayon::green("informs taxonomically features")
)
log_debug(
  "Authors: ",
  crayon::green("AR"),
  ",",
  crayon::blue("PMA"),
  "\n"
)
log_debug("Contributors: ...")

# prepare_taxa()
targets::tar_make(names = matches("tax_pre"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
