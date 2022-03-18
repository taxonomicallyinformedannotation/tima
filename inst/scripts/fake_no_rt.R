start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

cat(
  "This script",
  crayon::green("fakes a feature table without retention time (rt) \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

#' TODO CLI DOCOPT

fake_no_rt()

fake_no_rt(
  input = "inst/extdata/interim/annotations/96fa7c88200e4a03bee4644e581e3fb0_isdb_filled.tsv.gz",
  output = "inst/extdata/interim/annotations/96fa7c88200e4a03bee4644e581e3fb0_isdb_filled_no_rt.tsv.gz"
)

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
