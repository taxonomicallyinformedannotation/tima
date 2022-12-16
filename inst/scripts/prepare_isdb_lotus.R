start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("Converts the predicted spectra from LOTUS \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

paths <- parse_yaml_paths()

#' @title Prepare LOTUS In Silico DataBase
#'
#' @param input_pos TODO
#' @param input_neg TODO
#' @param output_pos TODO
#' @param output_neg TODO
#'
#' @return TODO
#'
#' @export
#'
#' @examples TODO
prepare_isdb_lotus <-
  function(input_pos = paths$data$source$spectra$lotus_isdb$pos,
           input_neg = paths$data$source$spectra$lotus_isdb$neg,
           output_pos = paths$data$interim$spectra$lotus_isdb$pos,
           output_neg = paths$data$interim$spectra$lotus_isdb$neg) {
    cat("TODO mgf sql conversion")
  }

prepare_isdb_lotus()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
