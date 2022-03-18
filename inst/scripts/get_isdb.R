start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

cat(
  "This script",
  crayon::green("downloads the In Silico DataBase (ISDB) \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

#' TODO CLI DOCOPT

get_isdb()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
