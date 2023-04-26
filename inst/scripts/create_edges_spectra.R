start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

# step <- "create_edges_spectra"
# paths <- parse_yaml_paths()
# params <- get_params(step = step)

log_debug(
  "This script",
  crayon::green("performs spectral similarity calculation to create edges. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ", crayon::blue("CH"), "\n")

# create_edges_spectra()
targets::tar_make(names = matches("fea_edg_spe"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
