start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

paths <- parse_yaml_paths()

log_debug(
  "This script",
  crayon::green("downloads an example of spectral_lib_matcher (ISDB) output. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

#' @title Get example ISDB
#'
#' @param url URL to the example ISDB
#' @param export Output file
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom readr read_tsv write_tsv
#'
#' @examples NULL
get_example_isdb <-
  function(url = paths$urls$example_isdb,
           export = paths$data$interim$annotations$example_isdb) {
    create_dir(export = export)

    readr::read_tsv(file = url) |>
      readr::write_tsv(file = export)
  }

get_example_isdb()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
