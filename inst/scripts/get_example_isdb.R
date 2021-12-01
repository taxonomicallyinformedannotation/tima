start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug("This script downloads an example of spectral_lib_matcher output. \n")
log_debug("Authors: AR")
log_debug("Contributors: ...")

paths <- parse_yaml_paths()

input <-
  "https://metabo-store.nprod.net/tima_example_files/interim/example_isdb_result.tsv.gz"

output <- "data/interim/annotations/example_isdb_result.tsv.gz"
readr::read_tsv(file = input) |>
  write_tsv(file = output)
