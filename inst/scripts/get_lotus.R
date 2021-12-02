start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

cat("This script downloads LOTUS referenced structure-organism pairs. \n")
cat("Authors: AR")
cat("Contributors: ...")

# TODO CLI DOCOPT

get_lotus()

end <- Sys.time()

log_debug("Script finished in", format(end - start))
