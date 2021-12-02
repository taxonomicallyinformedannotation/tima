start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug("This script downloads an example of spectral_lib_matcher (ISDB) output. \n")
log_debug("Authors: AR")
log_debug("Contributors: ...")

# TODO CLI DOCOPT

get_example_isdb()

end <- Sys.time()

log_debug("Script finished in", format(end - start))
